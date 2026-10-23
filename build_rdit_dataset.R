library(tidyverse)
library(lubridate)

# ── Configuration ─────────────────────────────────────────────────────────────

DATA_DIR <- "fec_bulk"
OUT_FILE <- "rdit_dataset.csv"
dir.create(DATA_DIR, showWarnings = FALSE)

WINDOW <- 30L  # days before and after each attack

# ── FEC bulk file column definitions ─────────────────────────────────────────

INDIV_COLS <- c(
  "CMTE_ID", "AMNDT_IND", "RPT_TP", "TRANSACTION_PGI", "IMAGE_NUM",
  "TRANSACTION_TP", "ENTITY_TP", "NAME", "CITY", "STATE", "ZIP_CODE",
  "EMPLOYER", "OCCUPATION", "TRANSACTION_DT", "TRANSACTION_AMT",
  "OTHER_ID", "TRAN_ID", "FILE_NUM", "MEMO_CD", "MEMO_TEXT", "SUB_ID"
)

# Only read the columns we need; skip everything else
INDIV_TYPES <- cols(
  CMTE_ID         = col_character(),
  TRANSACTION_TP  = col_character(),
  TRANSACTION_DT  = col_character(),
  TRANSACTION_AMT = col_double(),
  MEMO_CD         = col_character(),
  .default        = col_skip()
)

CCL_COLS <- c(
  "CAND_ID", "CAND_ELECTION_YR", "FEC_ELECTION_YR",
  "CMTE_ID", "CMTE_TP", "CMTE_DSGN", "LINKAGE_ID"
)

WEBALL_COLS <- c(
  "CAND_ID", "CAND_NAME", "CAND_ICI", "PTY_CD", "CAND_PTY_AFFILIATION",
  "TTL_RECEIPTS", "TRANS_FROM_AUTH", "TTL_DISB", "TRANS_TO_AUTH",
  "COH_BOP", "COH_COP", "CAND_CONTRIB", "CAND_LOANS", "OTHER_LOANS",
  "CAND_LOAN_REPAY", "OTHER_LOAN_REPAY", "DEBTS_OWED_BY", "TTL_INDIV_CONTRIB",
  "CAND_OFFICE_ST", "CAND_OFFICE_DISTRICT", "SPEC_ELECTION", "PRIM_ELECTION",
  "RUN_ELECTION", "GEN_ELECTION", "GEN_ELECTION_PRECENT", "OTHER_POL_CMTE_CONTRIB",
  "POL_PTY_CONTRIB", "CVG_END_DT", "INDIV_REFUNDS", "CMTE_REFUNDS"
)

# ── Helper: download and cache small bulk files (weball, ccl) ────────────────

get_bulk_file <- function(url, col_names, col_types = cols(.default = col_character())) {
  zip_path <- file.path(DATA_DIR, basename(url))

  if (!file.exists(zip_path)) {
    cat(sprintf("  Downloading %s ...\n", basename(url)))
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
  } else {
    cat(sprintf("  Cached: %s\n", basename(url)))
  }

  # List zip contents — internal filenames are not always predictable
  zip_contents <- unzip(zip_path, list = TRUE)
  txt_name <- zip_contents$Name[str_ends(zip_contents$Name, ".txt")][1]
  if (is.na(txt_name))
    stop(sprintf("No .txt file found inside %s", basename(url)))

  txt_path <- file.path(DATA_DIR, txt_name)
  if (!file.exists(txt_path))
    unzip(zip_path, files = txt_name, exdir = DATA_DIR)

  read_delim(txt_path, delim = "|", col_names = col_names,
             col_types = col_types, show_col_types = FALSE, progress = FALSE)
}

# ── Helper: stream large indiv files without writing to disk ──────────────────
# Pipes curl → unzip → readr chunked reader.
# The FEC individual contributions file is always named itcont.txt inside the zip.
# No compressed or uncompressed file is written to disk — only filtered rows
# are kept in memory.

INDIV_TYPES_CHUNKED <- cols_only(
  CMTE_ID         = col_character(),
  TRANSACTION_TP  = col_character(),
  TRANSACTION_DT  = col_character(),
  TRANSACTION_AMT = col_double(),
  MEMO_CD         = col_character(),
  OTHER_ID        = col_character()   # earmark recipient for WinRed conduit records
)

# WinRed (C00694323) is a conduit PAC that itemizes ALL earmarked donations,
# including sub-$200 amounts, as MEMO items on the recipient candidate committee's
# own Schedule A filing. These appear with MEMO_CD = 'X' and OTHER_ID = C00694323.
# We capture them by reading the recipient committee's INDIV file rows where
# OTHER_ID == WINRED_CMTE_ID, giving us daily small-dollar receipt data.
WINRED_CMTE_ID <- "C00694323"

stream_indiv <- function(url, target_cmtes, target_cand_ids, winred_id, cycle_yr) {
  # zip format stores its index at the END of the file, so unzip cannot
  # decompress from a pipe. Instead: download compressed zip to a temp file
  # (no unzipping — only the compressed bytes hit disk), read with unz(),
  # then immediately delete. Peak disk use = size of one compressed zip.
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)

  cat(sprintf("  Downloading %s (compressed only)...\n", basename(url)))
  dl_ok <- tryCatch({
    withr::with_options(
      list(timeout = 3600L),
      download.file(url, tmp, mode = "wb", quiet = TRUE)
    )
    TRUE
  }, error = function(e) {
    message("  Download failed: ", conditionMessage(e))
    FALSE
  })
  if (!dl_ok || !file.exists(tmp) || file.size(tmp) == 0) {
    warning(sprintf("Empty or missing download for %s", basename(url)))
    return(NULL)
  }
  cat(sprintf("  Downloaded (%.1f GB compressed).\n", file.size(tmp) / 1e9))

  # Identify the .txt file inside the zip
  zip_contents <- unzip(tmp, list = TRUE)
  txt_name <- zip_contents$Name[str_ends(zip_contents$Name, ".txt")][1]
  if (is.na(txt_name)) {
    warning(sprintf("No .txt file found inside %s", basename(url)))
    return(NULL)
  }
  cat(sprintf("  Reading '%s' via unz() connection (chunked)...\n", txt_name))

  con <- unz(tmp, txt_name)
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  result <- tryCatch(
    read_delim_chunked(
      con,
      delim          = "|",
      col_names      = INDIV_COLS,
      col_types      = INDIV_TYPES_CHUNKED,
      chunk_size     = 100000L,
      callback       = DataFrameCallback$new(function(chunk, pos) {
        chunk |>
          filter(
            # Keep three classes of records:
            # (1) Direct itemized individual contributions to target candidate committees
            #     (non-memo items only, to avoid double-counting conduit pass-throughs)
            # (2) WinRed earmarked contributions reported as memo items on candidate
            #     committee filings: CMTE_ID = candidate committee, OTHER_ID = WinRed,
            #     MEMO_CD = 'X'. These capture sub-$200 WinRed donations that the
            #     candidate committee itemizes as earmarked memo entries.
            #     FEC docs: OTHER_ID is null for individual contributors; for conduit
            #     earmarks reported by the RECIPIENT committee, OTHER_ID = conduit ID.
            # Accept target committee rows that are either:
            #  - non-memo items (direct contributions), OR
            #  - WinRed earmark memo items (MEMO_CD='X', OTHER_ID = WinRed conduit ID)
            CMTE_ID %in% target_cmtes,
            (is.na(MEMO_CD) | MEMO_CD != "X") | OTHER_ID == winred_id,
            str_starts(TRANSACTION_TP, "15") | TRANSACTION_TP == "10",
            TRANSACTION_AMT > 0
          ) |>
          mutate(
            date         = mdy(TRANSACTION_DT),
            # committee_id is always CMTE_ID (the candidate committee)
            committee_id = CMTE_ID,
            source       = if_else(!is.na(OTHER_ID) & OTHER_ID == winred_id,
                                   "winred", "candidate")
          ) |>
          filter(!is.na(date)) |>
          select(committee_id, date, amount = TRANSACTION_AMT, source)
      }),
      show_col_types = FALSE,
      progress       = FALSE
    ),
    error = function(e) {
      message("  Read error: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    warning(sprintf("No matching records from %s", basename(url)))
    return(NULL)
  }

  cat(sprintf("  → %s records matched.\n", scales::comma(nrow(result))))
  mutate(result, cycle = cycle_yr)
}

# ── FEC quarterly filing deadlines, 2019–2027 ────────────────────────────────
# Q1 = Apr 15, Q2 = Jul 15, Q3 = Oct 15, Q4 = Jan 31 (following year).
# Weekend adjustment: Saturday → +2, Sunday → +1.

next_business_day <- function(d) {
  d + case_when(wday(d) == 7L ~ 2L, wday(d) == 1L ~ 1L, TRUE ~ 0L)
}

fec_deadlines <- expand_grid(year = 2019:2027, quarter = 1:4) |>
  mutate(deadline = case_when(
    quarter == 1L ~ as.Date(paste0(year,      "-04-15")),
    quarter == 2L ~ as.Date(paste0(year,      "-07-15")),
    quarter == 3L ~ as.Date(paste0(year,      "-10-15")),
    quarter == 4L ~ as.Date(paste0(year + 1L, "-01-31"))
  )) |>
  mutate(deadline = map_vec(deadline, next_business_day)) |>
  pull(deadline) |>
  sort()

days_to_next_deadline <- function(date) {
  upcoming <- fec_deadlines[fec_deadlines >= date]
  if (length(upcoming) == 0L) return(NA_integer_)
  as.integer(min(upcoming) - date)
}

# ── Load RINO posts (treated candidates only) ─────────────────────────────────

posts    <- read_csv("rino_posts.csv",                show_col_types = FALSE)
fed_offs <- read_csv("rino_federal_officeholders.csv", show_col_types = FALSE)
# Presidential candidates excluded from rino_federal_officeholders.csv

# Derive attack_venue from platform field
# Currently all observations are Social Media; column kept for future expansion
posts <- posts |>
  mutate(
    attack_date  = as.Date(date_time),
    attack_venue = case_when(
      platform %in% c("Twitter", "Truth Social") ~ "Social Media",
      TRUE ~ "Other"
    )
  )

# Federal treated posts: one row per post × officeholder combination
fed_posts <- posts |>
  inner_join(fed_offs |> select(officeholder), by = "officeholder") |>
  select(post_id, officeholder, attack_date, attack_venue) |>
  distinct()

cat(sprintf("Treated federal posts: %d events, %d officials\n",
            nrow(fed_posts), n_distinct(fed_posts$officeholder)))

# ── Phase 1: Resolve FEC candidate and committee IDs ─────────────────────────

cat("\n=== Phase 1: Resolving candidate and committee IDs ===\n")

# House and Senate only (presidential candidates excluded)
name_lookup <- tribble(
  ~officeholder,            ~fec_name_pattern,          ~office_prefix, ~state,
  "Mitt Romney",            "ROMNEY, MITT",              "S", "UT",
  "Pat Toomey",             "TOOMEY, PAT",               "S", "PA",
  "Ben Sasse",              "SASSE, BEN",                "S", "NE",
  "Larry Hogan",            "HOGAN, LARRY",              "S", "MD",
  "John Thune",             "THUNE, JOHN",               "S", "SD",
  "Nancy Mace",             "MACE, NANCY",               "H", "SC",
  "Pete Ricketts",          "RICKETTS, PETE",            "S", "NE",
  "John Cornyn",            "CORNYN, JOHN",              "S", "TX",
  "Peter Meijer",           "MEIJER, PETER",             "H", "MI",
  "Mitch McConnell",        "MCCONNELL, MITCH",          "S", "KY",
  "Liz Cheney",             "CHENEY, LIZ",               "H", "WY",
  "Bill Cassidy",           "CASSIDY, BILL",             "S", "LA",
  "Lisa Murkowski",         "MURKOWSKI, LISA",           "S", "AK",
  "Richard Burr",           "BURR, RICHARD",             "S", "NC",
  "Susan Collins",          "COLLINS, SUSAN",            "S", "ME",
  "Joe O'Dea",              "O'DEA, JOE",                "S", "CO",
  "Todd Young",             "YOUNG, TODD",               "S", "IN",
  "Fred Upton",             "UPTON, FRED",               "H", "MI",
  "Tom Emmer",              "EMMER, TOM",                "H", "MN",
  "Ken Buck",               "BUCK, KEN",                 "H", "CO",
  "Chip Roy",               "ROY, CHIP",                 "H", "TX",
  "Kelly Daughtry",         "DAUGHTRY, KELLY",           "H", "NC",
  "Dan Newhouse",           "NEWHOUSE, DAN",             "H", "WA",
  "Thomas Massie",          "MASSIE, THOMAS",            "H", "KY",
  "Marjorie Taylor Greene", "GREENE, MARJORIE",          "H", "GA",
  "Jeff Hurd",              "HURD, JEFF",                "H", "CO",
  "John Thune",             "THUNE, JOHN",               "S", "SD"
) |>
  distinct(officeholder, .keep_all = TRUE)

# Cycles required (based on attack dates; cycle = next even year)
post_cycles <- fed_posts |>
  mutate(cycle = if_else(year(attack_date) %% 2L == 0L,
                         year(attack_date),
                         year(attack_date) + 1L)) |>
  pull(cycle) |> unique() |> sort()

cat("Cycles needed:", paste(post_cycles, collapse = ", "), "\n")

# Download weball and ccl files
weball_all <- map(post_cycles, \(yr) {
  yy  <- substr(yr, 3, 4)
  url <- sprintf("https://www.fec.gov/files/bulk-downloads/%d/weball%s.zip", yr, yy)
  df  <- get_bulk_file(url, WEBALL_COLS)
  df$CYCLE <- yr; df
}) |> bind_rows()

# CCL zips often contain a generic "ccl.txt" (no year suffix), causing later
# cycles to read stale data if the file already exists. Extract each to a
# cycle-stamped filename to prevent collisions.
read_ccl_cycle <- function(yr) {
  yy       <- substr(yr, 3, 4)
  zip_path <- file.path(DATA_DIR, sprintf("ccl%s.zip", yy))

  if (!file.exists(zip_path)) {
    cat(sprintf("  Downloading ccl%s.zip ...\n", yy))
    download.file(
      sprintf("https://www.fec.gov/files/bulk-downloads/%d/ccl%s.zip", yr, yy),
      zip_path, mode = "wb", quiet = TRUE
    )
  } else {
    cat(sprintf("  Cached: ccl%s.zip\n", yy))
  }

  zip_contents <- unzip(zip_path, list = TRUE)
  txt_in_zip   <- zip_contents$Name[str_ends(zip_contents$Name, ".txt")][1]
  stamped_path <- file.path(DATA_DIR, sprintf("ccl_%d.txt", yr))

  if (!file.exists(stamped_path)) {
    tmp <- tempfile()
    unzip(zip_path, files = txt_in_zip, exdir = dirname(tmp))
    file.rename(file.path(dirname(tmp), txt_in_zip), stamped_path)
  }

  read_delim(stamped_path, delim = "|", col_names = CCL_COLS,
             col_types = cols(.default = col_character()),
             show_col_types = FALSE) |>
    mutate(CYCLE = yr)
}

ccl_all <- map(post_cycles, read_ccl_cycle) |>
  bind_rows() |>
  filter(CMTE_DSGN == "P")

# Match officeholders → candidate IDs
match_cand_id <- function(pattern, prefix, state, data) {
  # Match on last name only to handle FEC full legal names
  # e.g. "ROMNEY, WILLARD MITT" not matched by full "ROMNEY, MITT"
  # Apostrophes stripped to handle e.g. "O'DEA" stored as "ODEA" in FEC
  last_name <- str_extract(pattern, "^[^,]+") |> str_remove_all("'")
  hits <- data |>
    filter(str_starts(CAND_NAME, regex(paste0("^", last_name, ","),
                                       ignore_case = TRUE)),
           str_starts(CAND_ID, prefix),
           CAND_PTY_AFFILIATION == "REP")
  if (!is.na(state)) hits <- filter(hits, CAND_OFFICE_ST == state)
  if (nrow(hits) == 0L) return(NA_character_)
  hits |> arrange(desc(CYCLE)) |> pull(CAND_ID) |> first()
}

candidates <- name_lookup |>
  mutate(candidate_id = pmap_chr(
    list(fec_name_pattern, office_prefix, state),
    match_cand_id, data = weball_all
  ))

unmatched <- filter(candidates, is.na(candidate_id))$officeholder
if (length(unmatched) > 0)
  cat("Unmatched:", paste(unmatched, collapse = ", "), "\n")
cat(sprintf("Matched %d of %d officials.\n",
            sum(!is.na(candidates$candidate_id)), nrow(candidates)))

# Candidate ID → most recent committee available at or before each attack cycle.
# This handles officials whose committee doesn't renew every cycle (e.g. Cheney
# in 2022 used the same committee as 2020, Buck's committee persisted after
# retirement). Officials with no federal committee in any cycle keep NA.
cmte_map <- candidates |>
  filter(!is.na(candidate_id)) |>
  left_join(ccl_all |> select(CAND_ID, CMTE_ID, CYCLE),
            by = c("candidate_id" = "CAND_ID")) |>
  rename(committee_id = CMTE_ID, cmte_cycle = CYCLE) |>
  filter(!is.na(committee_id)) |>
  distinct() |>
  select(officeholder, candidate_id, committee_id, cmte_cycle)

# For each post, pick the most recent committee at or before the attack cycle
fed_posts_cmte <- fed_posts |>
  mutate(cycle = if_else(year(attack_date) %% 2L == 0L,
                         year(attack_date),
                         year(attack_date) + 1L)) |>
  left_join(cmte_map, by = "officeholder", relationship = "many-to-many") |>
  filter(is.na(cmte_cycle) | cmte_cycle <= cycle) |>
  group_by(post_id, officeholder, cycle, attack_date, attack_venue) |>
  slice_max(order_by = cmte_cycle, n = 1, with_ties = FALSE) |>
  ungroup() |>
  # Re-attach posts for officials with no committee at all (→ NA receipts)
  right_join(
    fed_posts |>
      mutate(cycle = if_else(year(attack_date) %% 2L == 0L,
                             year(attack_date),
                             year(attack_date) + 1L)) |>
      select(post_id, officeholder, cycle, attack_date, attack_venue),
    by = c("post_id", "officeholder", "cycle", "attack_date", "attack_venue")
  ) |>
  select(post_id, officeholder, candidate_id, committee_id,
         cycle, attack_date, attack_venue)

# Load CEM-matched control committee IDs (produced by the CEM matching step)
# cem_matched_data.csv and control_cmte_map.csv must exist before running.
cem_matches    <- read_csv("cem_matched_data.csv",  show_col_types = FALSE)
control_cmte   <- read_csv("control_cmte_map.csv",  show_col_types = FALSE)

# Extract control event structure: for each treated post, controls in the
# same CEM subclass × cycle serve as the comparison group, observed over
# the same 61-day calendar window.
treated_subclass <- fed_posts_cmte |>
  filter(!is.na(committee_id)) |>
  left_join(
    cem_matches |>
      filter(is_treated == 1) |>
      select(bioname, cycle, subclass),
    by = c("officeholder" = "bioname", "cycle")
  ) |>
  filter(!is.na(subclass))

control_events <- cem_matches |>
  filter(is_treated == 0) |>
  select(bioguide_id, bioname, cycle, subclass, weights) |>
  left_join(control_cmte |> select(bioguide_id, committee_id),
            by = "bioguide_id") |>
  filter(!is.na(committee_id))

# For each treated post, assign all same-subclass controls its attack date
control_posts <- treated_subclass |>
  select(subclass, post_id, attack_date, attack_venue, cycle) |>
  distinct() |>
  inner_join(control_events, by = c("subclass", "cycle"),
             relationship = "many-to-many") |>
  select(bioguide_id, officeholder = bioname, committee_id,
         cycle, subclass, post_id, attack_date, attack_venue, weights)

target_cmtes    <- na.omit(unique(c(fed_posts_cmte$committee_id,
                                    control_cmte$committee_id)))
target_cand_ids <- na.omit(unique(fed_posts_cmte$candidate_id))
cat(sprintf("\n%d target committees (%d treated + %d control) | %d candidate IDs.\n",
            length(target_cmtes),
            n_distinct(na.omit(fed_posts_cmte$committee_id)),
            n_distinct(control_cmte$committee_id),
            length(target_cand_ids)))

# ── Phase 2: Extract Schedule A transactions ──────────────────────────────────

cat("\n=== Phase 2: Extracting Schedule A individual contributions ===\n")
cat("NOTE: daily_total_receipts reflects itemized individual contributions only\n",
    "      (Schedule A). PAC, party, and unitemized contributions excluded.\n\n")

indiv_all <- map(post_cycles, \(yr) {
  yy  <- substr(yr, 3, 4)
  url <- sprintf("https://www.fec.gov/files/bulk-downloads/%d/indiv%s.zip", yr, yy)
  cat(sprintf("\nCycle %d:\n", yr))
  stream_indiv(url, target_cmtes, target_cand_ids, WINRED_CMTE_ID, yr)
}) |>
  compact() |>   # drop NULLs from any failed cycles
  bind_rows()

cat(sprintf("Extracted %s individual contribution records.\n",
            scales::comma(nrow(indiv_all))))

# ── Phase 3: Aggregate to daily committee totals ──────────────────────────────
# committee_id is already the candidate committee ID for both direct and WinRed
# records (no remapping needed — WinRed earmarks are read from the recipient
# committee's own filing, not from WinRed's conduit filing).

# Direct Schedule A contributions to each candidate committee
daily_totals <- indiv_all |>
  filter(source == "candidate") |>
  group_by(committee_id, date) |>
  summarise(daily_total_receipts = sum(amount, na.rm = TRUE), .groups = "drop")

# WinRed conduit: sub-$200 donations earmarked to each candidate
# NOTE: WinRed launched 2019; coverage strongest from 2020 onward.
daily_small_dollar <- indiv_all |>
  filter(source == "winred", amount < 200) |>
  group_by(committee_id, date) |>
  summarise(daily_small_dollar_receipts = sum(amount, na.rm = TRUE), .groups = "drop")

# ── Phase 4: Build the RDiT panel ────────────────────────────────────────────

cat("\n=== Phase 4: Building RDiT panel ===\n")

build_windows <- function(events_df) {
  events_df |>
    mutate(date = map(attack_date, \(d) seq(d - WINDOW, d + WINDOW, by = "day"))) |>
    unnest(date)
}

fill_receipts <- function(df) {
  df |>
    left_join(daily_totals,       by = c("committee_id", "date")) |>
    left_join(daily_small_dollar, by = c("committee_id", "date")) |>
    mutate(
      daily_total_receipts = if_else(
        is.na(committee_id), NA_real_, replace_na(daily_total_receipts, 0)
      ),
      daily_small_dollar_receipts = if_else(
        is.na(committee_id), NA_real_, replace_na(daily_small_dollar_receipts, 0)
      ),
      days_from_attack     = as.integer(date - attack_date),
      post_attack          = if_else(days_from_attack >= 0L, 1L, 0L),
      days_to_fec_deadline = map_int(date, days_to_next_deadline)
    )
}

PANEL_COLS <- c("candidate_id", "committee_id", "officeholder", "cycle",
                "post_id", "attack_date", "attack_venue", "is_treated",
                "subclass", "weights", "date", "days_from_attack",
                "post_attack", "daily_total_receipts",
                "daily_small_dollar_receipts", "days_to_fec_deadline")

# Treated rows
treated_panel <- fed_posts_cmte |>
  left_join(
    cem_matches |> filter(is_treated == 1) |>
      select(bioname, cycle, subclass, weights),
    by = c("officeholder" = "bioname", "cycle")
  ) |>
  build_windows() |>
  fill_receipts() |>
  mutate(is_treated = 1L) |>
  select(any_of(PANEL_COLS))

# Control rows — pseudo-attack date assigned from matched treated post
control_panel <- control_posts |>
  mutate(candidate_id = NA_character_) |>
  build_windows() |>
  fill_receipts() |>
  mutate(is_treated = 0L) |>
  select(any_of(PANEL_COLS))

rdit_panel <- bind_rows(treated_panel, control_panel) |>
  arrange(is_treated, officeholder, attack_date, date)

write_csv(rdit_panel, OUT_FILE)

cat(sprintf("RDiT panel: %s rows × %d cols → %s\n",
            scales::comma(nrow(rdit_panel)), ncol(rdit_panel), OUT_FILE))
cat(sprintf("  Treated officials: %d | Post events: %d\n",
            n_distinct(treated_panel$officeholder),
            n_distinct(treated_panel$post_id)))
cat(sprintf("  Control officials: %d\n",
            n_distinct(control_panel$officeholder)))

rdit_panel |>
  group_by(is_treated, post_attack) |>
  summarise(n = n(), mean_daily_receipts = mean(daily_total_receipts,
                                                 na.rm = TRUE),
            .groups = "drop") |>
  print()
