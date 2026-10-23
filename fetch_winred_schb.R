# fetch_winred_schb.R
#
# Extracts WinRed (C00694323) Schedule B disbursement records from the FEC's
# "Other Committee Transactions" bulk files (pas2{yy}.zip) and uses them to
# populate daily_small_dollar_receipts in rdit_dataset.csv.
#
# Why pas2 instead of re-running build_rdit_dataset.R:
#   - pas2 files are ~50–200 MB compressed (vs 3–4 GB for indiv files per cycle)
#   - WinRed's Schedule B records show exactly what it forwarded to each
#     candidate committee on each date — same signal, far lighter download
#
# Run as a background job: Tools → Background Jobs → Start Job
# Estimated time: 5–20 minutes depending on connection speed.
#
# Output: updates rdit_dataset.csv in place with populated
#         daily_small_dollar_receipts column.

library(tidyverse)

DATA_DIR <- "fec_bulk"
WINRED_CMTE_ID <- "C00694323"
CYCLES <- c(2020L, 2022L, 2024L, 2026L)

# pas2 column names (same pipe-delimited format as indiv/itcont files)
PAS2_COLS <- c(
  "CMTE_ID", "AMNDT_IND", "RPT_TP", "TRANSACTION_PGI", "IMAGE_NUM",
  "TRANSACTION_TP", "ENTITY_TP", "NAME", "CITY", "STATE", "ZIP_CODE",
  "EMPLOYER", "OCCUPATION", "TRANSACTION_DT", "TRANSACTION_AMT",
  "OTHER_ID", "CAND_ID", "TRAN_ID", "FILE_NUM", "MEMO_CD", "MEMO_TEXT", "SUB_ID"
)

PAS2_TYPES <- cols_only(
  CMTE_ID         = col_character(),
  TRANSACTION_TP  = col_character(),
  TRANSACTION_DT  = col_character(),
  TRANSACTION_AMT = col_double(),
  OTHER_ID        = col_character(),   # recipient committee
  CAND_ID         = col_character(),   # recipient candidate
  MEMO_CD         = col_character()
)

# ── Load target committees ────────────────────────────────────────────────────

cat("Loading rdit_dataset.csv...\n")
rdit <- read_csv("rdit_dataset.csv", show_col_types = FALSE)

# All committee IDs in the panel (treated + control)
target_cmtes <- rdit |>
  filter(!is.na(committee_id)) |>
  distinct(committee_id)

cat(sprintf("Target committees: %d total (%d treated, %d control)\n",
            nrow(target_cmtes),
            n_distinct(rdit$committee_id[rdit$is_treated == 1 & !is.na(rdit$committee_id)]),
            n_distinct(rdit$committee_id[rdit$is_treated == 0 & !is.na(rdit$committee_id)])))

# ── Download and parse pas2 files ─────────────────────────────────────────────

cat("\n=== Downloading FEC pas2 (committee disbursement) files ===\n")

winred_schb <- map(CYCLES, function(yr) {
  yy       <- substr(yr, 3, 4)
  zip_url  <- sprintf("https://www.fec.gov/files/bulk-downloads/%d/pas2%s.zip", yr, yy)
  zip_path <- file.path(DATA_DIR, sprintf("pas2%s.zip", yy))
  txt_path <- file.path(DATA_DIR, sprintf("pas2_%d.txt", yr))  # year-stamped to avoid collisions

  # ── Download ────────────────────────────────────────────────────────────────
  if (!file.exists(zip_path)) {
    cat(sprintf("\n[%d] Downloading %s ...\n", yr, basename(zip_url)))
    dl_ok <- tryCatch({
      withr::with_options(
        list(timeout = 3600L),
        download.file(zip_url, zip_path, mode = "wb", quiet = FALSE)
      )
      TRUE
    }, error = function(e) {
      message(sprintf("  Download failed: %s", conditionMessage(e)))
      FALSE
    })

    if (!dl_ok || file.size(zip_path) == 0) {
      warning(sprintf("Could not download %s — skipping cycle %d", basename(zip_url), yr))
      return(NULL)
    }
  } else {
    cat(sprintf("\n[%d] Cached: %s (%.1f MB)\n", yr, basename(zip_path),
                file.size(zip_path) / 1e6))
  }

  # ── Unzip ───────────────────────────────────────────────────────────────────
  if (!file.exists(txt_path)) {
    zip_contents <- unzip(zip_path, list = TRUE)
    txt_in_zip   <- zip_contents$Name[str_ends(zip_contents$Name, ".txt")][1]
    if (is.na(txt_in_zip)) {
      warning(sprintf("No .txt inside %s — skipping cycle %d", basename(zip_path), yr))
      return(NULL)
    }
    unzip(zip_path, files = txt_in_zip, exdir = DATA_DIR)
    extracted <- file.path(DATA_DIR, txt_in_zip)
    if (extracted != txt_path) file.rename(extracted, txt_path)
    cat(sprintf("  Unzipped → %s (%.1f MB)\n", basename(txt_path),
                file.size(txt_path) / 1e6))
  } else {
    cat(sprintf("  Unzipped file already exists: %s\n", basename(txt_path)))
  }

  # ── Read and filter ─────────────────────────────────────────────────────────
  cat(sprintf("  Reading and filtering for WinRed...\n"))
  df <- tryCatch(
    read_delim(
      txt_path,
      delim      = "|",
      col_names  = PAS2_COLS,
      col_types  = PAS2_TYPES,
      show_col_types = FALSE,
      progress   = FALSE
    ),
    error = function(e) {
      message(sprintf("  Read error on %s: %s", basename(txt_path), conditionMessage(e)))
      NULL
    }
  )

  if (is.null(df)) return(NULL)

  winred_rows <- df |>
    filter(CMTE_ID == WINRED_CMTE_ID, TRANSACTION_AMT > 0, !is.na(TRANSACTION_AMT))

  cat(sprintf("  → %s WinRed rows found in pas2%s\n",
              scales::comma(nrow(winred_rows)), yy))

  # Parse dates
  winred_rows |>
    mutate(date = lubridate::mdy(TRANSACTION_DT), cycle = yr) |>
    filter(!is.na(date))

}) |>
  compact() |>
  bind_rows()

cat(sprintf("\n=== WinRed Schedule B extraction complete ===\n"))
cat(sprintf("Total WinRed disbursement records: %s\n", scales::comma(nrow(winred_schb))))

if (nrow(winred_schb) == 0) {
  stop("No WinRed records found in pas2 files. Check that CMTE_ID = C00694323 is correct.")
}

# ── Diagnostics ───────────────────────────────────────────────────────────────

cat("\nTransaction types used by WinRed:\n")
winred_schb |>
  count(TRANSACTION_TP, sort = TRUE) |>
  print(n = 20)

cat("\nDate range of WinRed records:", as.character(range(winred_schb$date)), "\n")

cat("\nAmount distribution of WinRed disbursements:\n")
summary(winred_schb$TRANSACTION_AMT) |> print()

# How do WinRed records identify their recipients?
# OTHER_ID = recipient committee; CAND_ID = recipient candidate
cat(sprintf("\nRecords with OTHER_ID (recipient committee): %s\n",
            scales::comma(sum(!is.na(winred_schb$OTHER_ID)))))
cat(sprintf("Records with CAND_ID (recipient candidate):   %s\n",
            scales::comma(sum(!is.na(winred_schb$CAND_ID)))))

# Check overlap with target committees via OTHER_ID
n_matched_other <- sum(winred_schb$OTHER_ID %in% target_cmtes$committee_id, na.rm = TRUE)
cat(sprintf("WinRed rows matching target committees (OTHER_ID): %s\n",
            scales::comma(n_matched_other)))

# ── Aggregate to daily committee totals ───────────────────────────────────────

cat("\n=== Aggregating daily WinRed small-dollar receipts ===\n")

# Match on OTHER_ID (recipient committee) — primary approach
# Some records may use CAND_ID instead; we'll check in diagnostics above
winred_daily <- winred_schb |>
  filter(
    OTHER_ID %in% target_cmtes$committee_id,
    # Exclude memo items if any are present (shouldn't be in Schedule B, but defensive)
    is.na(MEMO_CD) | MEMO_CD != "X"
  ) |>
  group_by(committee_id = OTHER_ID, date) |>
  summarise(
    daily_small_dollar_receipts = sum(TRANSACTION_AMT, na.rm = TRUE),
    n_disbursements             = n(),
    .groups                     = "drop"
  )

cat(sprintf("Committee-days with WinRed disbursements: %s\n",
            scales::comma(nrow(winred_daily))))
cat(sprintf("Committees matched: %d of %d target committees\n",
            n_distinct(winred_daily$committee_id),
            nrow(target_cmtes)))

if (nrow(winred_daily) > 0) {
  cat(sprintf("Total WinRed → target committees: %s\n",
              scales::dollar(sum(winred_daily$daily_small_dollar_receipts))))
  cat(sprintf("Date range: %s to %s\n",
              as.character(min(winred_daily$date)),
              as.character(max(winred_daily$date))))

  cat("\nTop 10 committee-days by WinRed receipts:\n")
  winred_daily |>
    left_join(rdit |> filter(is_treated == 1) |>
                distinct(committee_id, officeholder),
              by = "committee_id") |>
    arrange(desc(daily_small_dollar_receipts)) |>
    slice_head(n = 10) |>
    mutate(daily_small_dollar_receipts = scales::dollar(daily_small_dollar_receipts)) |>
    print()
}

# ── Update rdit_dataset.csv ───────────────────────────────────────────────────

cat("\n=== Updating rdit_dataset.csv ===\n")

rdit_updated <- rdit |>
  select(-daily_small_dollar_receipts) |>
  left_join(
    winred_daily |> select(committee_id, date, daily_small_dollar_receipts),
    by = c("committee_id", "date")
  ) |>
  mutate(
    daily_small_dollar_receipts = case_when(
      is.na(committee_id) ~ NA_real_,
      TRUE                ~ replace_na(daily_small_dollar_receipts, 0)
    )
  )

# Sanity check before writing
n_nonzero <- sum(rdit_updated$daily_small_dollar_receipts > 0, na.rm = TRUE)
total_sd  <- sum(rdit_updated$daily_small_dollar_receipts, na.rm = TRUE)

cat(sprintf("Rows with non-zero small-dollar receipts: %s\n", scales::comma(n_nonzero)))
cat(sprintf("Total small-dollar receipts recorded:     %s\n", scales::dollar(total_sd)))

if (n_nonzero == 0) {
  warning(paste(
    "No non-zero small-dollar receipts after join.",
    "Check that OTHER_ID in pas2 file matches committee_id format in rdit_dataset.",
    "Consider whether WinRed uses CAND_ID instead of OTHER_ID for recipient identification."
  ))
} else {
  write_csv(rdit_updated, "rdit_dataset.csv")
  cat("\nrdit_dataset.csv updated successfully.\n")
}

cat("\nDone.\n")
