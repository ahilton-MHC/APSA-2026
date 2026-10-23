library(tidyverse)

# ── FEC Bulk Data Downloader ──────────────────────────────────────────────────
# Uses FEC's publicly available bulk data files — no API key required.
# Files are downloaded once and cached locally.
#
# Produces:
#   fec_candidates.csv     — candidate IDs and basic info
#   fec_cycle_totals.csv   — fundraising totals per candidate per cycle
#   fec_quarterly.csv      — fundraising per filing period (for RDiT)

DATA_DIR <- "fec_bulk"
dir.create(DATA_DIR, showWarnings = FALSE)

# Election cycles to pull (two-year cycles ending in even year)
CYCLES <- c(2020, 2022, 2024, 2026)

# ── FEC bulk file column definitions ─────────────────────────────────────────

# weball: Candidate summary — one row per candidate per cycle
# https://www.fec.gov/campaign-finance-data/all-candidates-file-description/
weball_cols <- c(
  "CAND_ID", "CAND_NAME", "CAND_ICI", "PTY_CD", "CAND_PTY_AFFILIATION",
  "TTL_RECEIPTS", "TRANS_FROM_AUTH", "TTL_DISB", "TRANS_TO_AUTH",
  "COH_BOP", "COH_COP", "CAND_CONTRIB", "CAND_LOANS", "OTHER_LOANS",
  "CAND_LOAN_REPAY", "OTHER_LOAN_REPAY", "DEBTS_OWED_BY", "TTL_INDIV_CONTRIB",
  "CAND_OFFICE_ST", "CAND_OFFICE_DISTRICT", "SPEC_ELECTION", "PRIM_ELECTION",
  "RUN_ELECTION", "GEN_ELECTION", "GEN_ELECTION_PRECENT", "OTHER_POL_CMTE_CONTRIB",
  "POL_PTY_CONTRIB", "CVG_END_DT", "INDIV_REFUNDS", "CMTE_REFUNDS"
)

# ccl: Candidate-Committee Linkage — maps candidate IDs to committee IDs
ccl_cols <- c(
  "CAND_ID", "CAND_ELECTION_YR", "FEC_ELECTION_YR", "CMTE_ID",
  "CMTE_TP", "CMTE_DSGN", "LINKAGE_ID"
)

# ── Helper: download and cache a bulk file ────────────────────────────────────

get_bulk_file <- function(url, col_names) {
  zip_path <- file.path(DATA_DIR, basename(url))

  if (!file.exists(zip_path)) {
    cat(sprintf("  Downloading %s...\n", basename(url)))
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
  } else {
    cat(sprintf("  Using cached %s\n", basename(url)))
  }

  # List zip contents — FEC internal filenames are not always predictable
  zip_contents <- unzip(zip_path, list = TRUE)
  txt_name <- zip_contents$Name[str_ends(zip_contents$Name, ".txt")][1]

  if (is.na(txt_name))
    stop(sprintf("No .txt file found inside %s", basename(url)))

  # Only extract if not already on disk
  txt_path <- file.path(DATA_DIR, txt_name)
  if (!file.exists(txt_path))
    unzip(zip_path, files = txt_name, exdir = DATA_DIR)

  read_delim(
    txt_path,
    delim          = "|",
    col_names      = col_names,
    col_types      = cols(.default = "c"),
    show_col_types = FALSE
  )
}

# ── Phase 1: Download candidate summary files ─────────────────────────────────

cat("=== Phase 1: Downloading candidate summary files ===\n")

weball_list <- map(CYCLES, function(yr) {
  yy  <- substr(yr, 3, 4)
  url <- sprintf("https://www.fec.gov/files/bulk-downloads/%d/weball%s.zip", yr, yy)
  df  <- get_bulk_file(url, weball_cols)
  df$CYCLE <- yr
  df
})

weball_all <- bind_rows(weball_list) |>
  mutate(
    across(c(TTL_RECEIPTS, TTL_DISB, TTL_INDIV_CONTRIB,
             OTHER_POL_CMTE_CONTRIB, COH_COP), as.numeric),
    CVG_END_DT = as.Date(CVG_END_DT, format = "%m/%d/%Y")
  )

cat(sprintf("Loaded %d candidate-cycle rows across %d cycles.\n\n",
            nrow(weball_all), length(CYCLES)))

# ── Phase 2: Match our officeholders to FEC candidates ───────────────────────

cat("=== Phase 2: Matching officeholders to FEC records ===\n")

# Candidate IDs we want (identified by searching the bulk files)
# Format: CAND_ID always begins with H (House), S (Senate), or P (Presidential)
# We match on CAND_NAME and restrict to Republican party + relevant office state

target_names <- tribble(
  ~officeholder,            ~fec_name_pattern,              ~office_prefix, ~state,
  "Mitt Romney",            "ROMNEY, MITT",                 "S",            "UT",
  "Pat Toomey",             "TOOMEY, PAT",                  "S",            "PA",
  "Ben Sasse",              "SASSE, BEN",                   "S",            "NE",
  "Larry Hogan",            "HOGAN, LARRY",                 "S",            "MD",
  "John Thune",             "THUNE, JOHN",                  "S",            "SD",
  "Nancy Mace",             "MACE, NANCY",                  "H",            "SC",
  "Pete Ricketts",          "RICKETTS, PETE",               "S",            "NE",
  "John Cornyn",            "CORNYN, JOHN",                 "S",            "TX",
  "Peter Meijer",           "MEIJER, PETER",                "H",            "MI",
  "Mitch McConnell",        "MCCONNELL, MITCH",             "S",            "KY",
  "Liz Cheney",             "CHENEY, LIZ",                  "H",            "WY",
  "Bill Cassidy",           "CASSIDY, BILL",                "S",            "LA",
  "Lisa Murkowski",         "MURKOWSKI, LISA",              "S",            "AK",
  "Richard Burr",           "BURR, RICHARD",                "S",            "NC",
  "Susan Collins",          "COLLINS, SUSAN",               "S",            "ME",
  "Joe O'Dea",              "O'DEA, JOE",                   "S",            "CO",
  "Todd Young",             "YOUNG, TODD",                  "S",            "IN",
  "Fred Upton",             "UPTON, FRED",                  "H",            "MI",
  "Tom Emmer",              "EMMER, TOM",                   "H",            "MN",
  "Ken Buck",               "BUCK, KEN",                    "H",            "CO",
  "Chip Roy",               "ROY, CHIP",                    "H",            "TX",
  "Kelly Daughtry",         "DAUGHTRY, KELLY",              "H",            "NC",
  "Dan Newhouse",           "NEWHOUSE, DAN",                "H",            "WA",
  "Thomas Massie",          "MASSIE, THOMAS",               "H",            "KY",
  "Marjorie Taylor Greene", "GREENE, MARJORIE",             "H",            "GA",
  "Jeff Hurd",              "HURD, JEFF",                   "H",            "CO"
)

match_candidate <- function(fec_name_pat, office_prefix, state, data) {
  # Match on last name only (before comma) to handle FEC's use of full legal names
  # e.g. "ROMNEY, WILLARD MITT" is not caught by fixed("ROMNEY, MITT")
  # Apostrophes are stripped to handle e.g. "O'DEA" stored as "ODEA" in FEC
  last_name <- str_extract(fec_name_pat, "^[^,]+") |> str_remove_all("'")
  hits <- data |>
    filter(
      str_starts(CAND_NAME, regex(paste0("^", last_name, ","), ignore_case = TRUE)),
      str_starts(CAND_ID, office_prefix),
      CAND_PTY_AFFILIATION == "REP"
    )
  if (!is.na(state)) hits <- filter(hits, CAND_OFFICE_ST == state)
  if (nrow(hits) == 0) return(NA_character_)
  # Return the candidate ID seen in the most recent cycle
  hits |> arrange(desc(CYCLE)) |> pull(CAND_ID) |> first()
}

# Use all cycles combined to find IDs
candidates <- target_names |>
  mutate(CAND_ID = pmap_chr(
    list(fec_name_pattern, office_prefix, state),
    match_candidate,
    data = weball_all
  ))

matched <- sum(!is.na(candidates$CAND_ID))
cat(sprintf("Matched %d of %d candidates.\n", matched, nrow(candidates)))
if (any(is.na(candidates$CAND_ID))) {
  cat("Unmatched:\n")
  candidates |> filter(is.na(CAND_ID)) |> pull(officeholder) |> cat(sep = "\n  ")
  cat("\n")
}

write_csv(candidates, "fec_candidates.csv")

# ── Phase 3: Cycle totals (for logit / survival dataset) ─────────────────────

cat("\n=== Phase 3: Building cycle totals ===\n")

cycle_totals <- weball_all |>
  inner_join(candidates |> filter(!is.na(CAND_ID)) |> select(officeholder, CAND_ID),
             by = "CAND_ID") |>
  select(
    officeholder,
    CAND_ID,
    cycle            = CYCLE,
    total_receipts   = TTL_RECEIPTS,
    indiv_contrib    = TTL_INDIV_CONTRIB,
    pac_contrib      = OTHER_POL_CMTE_CONTRIB,
    total_disb       = TTL_DISB,
    cash_on_hand_eop = COH_COP,
    coverage_end     = CVG_END_DT
  ) |>
  arrange(officeholder, cycle)

write_csv(cycle_totals, "fec_cycle_totals.csv")
cat(sprintf("Cycle totals: %d rows → fec_cycle_totals.csv\n", nrow(cycle_totals)))

# ── Phase 4: Quarterly filing data (for RDiT dataset) ────────────────────────
# FEC publishes committee filing summaries in bulk: 
# https://www.fec.gov/campaign-finance-data/electronic-filing-bulk-downloads/
# The "webk" file has committee summary data per filing period.

cat("\n=== Phase 4: Quarterly filing data ===\n")

# ccl file maps candidate IDs → principal campaign committee IDs
ccl_list <- map(CYCLES, function(yr) {
  yy  <- substr(yr, 3, 4)
  url <- sprintf("https://www.fec.gov/files/bulk-downloads/%d/ccl%s.zip", yr, yy)
  df  <- get_bulk_file(url, ccl_cols)
  df$CYCLE <- yr
  df
})
ccl_all <- bind_rows(ccl_list) |>
  filter(CMTE_DSGN == "P")  # "P" = principal campaign committee

# Get committee IDs for our candidates
cmte_ids <- candidates |>
  filter(!is.na(CAND_ID)) |>
  left_join(ccl_all |> select(CAND_ID, CMTE_ID, CYCLE),
            by = "CAND_ID") |>
  select(officeholder, CAND_ID, CMTE_ID, CYCLE) |>
  distinct()

# Committee periodic summary (webk) — one row per committee per filing period
# Column definitions: https://www.fec.gov/campaign-finance-data/committee-summary-file-description/
webk_cols <- c(
  "CMTE_ID", "CMTE_NM", "CMTE_TP", "CMTE_DSGN", "CMTE_FILING_FREQ",
  "TTL_RECEIPTS", "TRANS_FROM_AFF", "INDV_CONTRIB", "OTHER_POL_CMTE_CONTRIB",
  "CAND_CONTRIB", "CAND_LOANS", "TTL_LOANS_RECEIVED", "TTL_DISB",
  "TRANF_TO_AFF", "INDV_REFUNDS", "OTHER_POL_CMTE_REFUNDS", "CAND_LOAN_REPAY",
  "LOAN_REPAY", "COH_BOP", "COH_COP", "DEBTS_OWED_BY", "NONFED_TRANS_RECEIVED",
  "CONTRIB_TO_OTHER_CMTE", "IND_EXP", "PTY_COORD_EXP", "NONFED_SHARE_EXP",
  "CVG_END_DT"
)

webk_list <- map(CYCLES, function(yr) {
  yy  <- substr(yr, 3, 4)
  url <- sprintf("https://www.fec.gov/files/bulk-downloads/%d/webk%s.zip", yr, yy)
  df  <- tryCatch(get_bulk_file(url, webk_cols), error = function(e) {
    message(sprintf("  Could not download webk%s: %s", yy, conditionMessage(e)))
    NULL
  })
  if (!is.null(df)) df$CYCLE <- yr
  df
})
webk_all <- bind_rows(compact(webk_list)) |>
  mutate(
    across(c(TTL_RECEIPTS, INDV_CONTRIB, OTHER_POL_CMTE_CONTRIB,
             TTL_DISB, COH_COP), as.numeric),
    CVG_END_DT = as.Date(CVG_END_DT, format = "%m/%d/%Y")
  )

quarterly <- webk_all |>
  inner_join(cmte_ids |> select(officeholder, CMTE_ID, CYCLE),
             by = c("CMTE_ID", "CYCLE")) |>
  select(
    officeholder,
    CMTE_ID,
    cycle            = CYCLE,
    coverage_end     = CVG_END_DT,
    total_receipts   = TTL_RECEIPTS,
    indiv_contrib    = INDV_CONTRIB,
    pac_contrib      = OTHER_POL_CMTE_CONTRIB,
    total_disb       = TTL_DISB,
    cash_on_hand_eop = COH_COP
  ) |>
  arrange(officeholder, coverage_end)

write_csv(quarterly, "fec_quarterly.csv")
cat(sprintf("Quarterly data: %d rows → fec_quarterly.csv\n", nrow(quarterly)))

cat("\n=== Done. Output files:\n")
cat("  fec_candidates.csv\n  fec_cycle_totals.csv\n  fec_quarterly.csv\n")
