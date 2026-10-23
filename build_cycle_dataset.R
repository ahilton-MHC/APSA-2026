library(tidyverse)
library(lubridate)

# ── build_cycle_dataset.R ─────────────────────────────────────────────────────
# Builds the cycle-level panel for logit and survival models.
#
# Final schema (per execution_rile.rtf):
#   candidate_id | cycle | chamber | is_treated | attack_count_quarter |
#   cook_pvi_num | seniority | committee_chair |
#   primary_challenger_entry | challenger_quality |
#   days_to_challenger_entry | incumbent_retired | primary_victory
#
# Prerequisites:
#   baseline_panel.csv   — treated + control members with seniority and Cook PVI
#                          Built in-session from Voteview + Wikipedia (run once)
#   rino_posts.csv       — RINO attack posts (for attack_count_quarter)
#
# Remaining manual sources:
#   Congress committee records    — committee_chair
#   FEC candidate filing records  — primary_challenger_entry, days_to_challenger_entry
#   Ballotpedia / ICPSR           — challenger_quality
#   Congress records / FEC        — incumbent_retired
#   MIT Election Data Lab         — primary_victory

DATA_DIR <- "fec_bulk"
OUT_FILE <- "cycle_dataset.csv"

# ── Load pre-built baseline panel ────────────────────────────────────────────
# baseline_panel.csv was built in-session from:
#   - Voteview member data (seniority)
#   - Wikipedia Cook PVI tables (cook_pvi_num)
# It contains both treated (is_treated=1) and control (is_treated=0) members.

baseline <- read_csv("baseline_panel.csv", show_col_types = FALSE) |>
  mutate(chamber = case_when(
    chamber == "House"  ~ 0L,
    chamber == "Senate" ~ 1L,
    TRUE ~ NA_integer_
  ))

posts    <- read_csv("rino_posts.csv",                show_col_types = FALSE)
fed_offs <- read_csv("rino_federal_officeholders.csv", show_col_types = FALSE)

# ── Section 1: Derive attack_count_quarter for treated candidates ─────────────

cat("=== Section 1: Deriving treatment variables ===\n")

fed_posts <- posts |>
  inner_join(fed_offs |> select(officeholder), by = "officeholder") |>
  mutate(
    attack_date  = as.Date(date_time),
    fec_year     = year(attack_date),
    fec_quarter  = quarter(attack_date),
    cycle        = if_else(fec_year %% 2L == 0L, fec_year, fec_year + 1L)
  )

# Maximum attack count within a quarter, per officeholder × cycle
quarter_counts <- fed_posts |>
  count(officeholder, cycle, fec_year, fec_quarter, name = "n_attacks") |>
  group_by(officeholder, cycle) |>
  summarise(attack_count_quarter = max(n_attacks),
            first_attack_date    = min(fed_posts$attack_date[
              fed_posts$officeholder == cur_group()$officeholder &
              fed_posts$cycle        == cur_group()$cycle]),
            .groups = "drop")

# Simpler: join counts back and collapse
quarter_counts <- fed_posts |>
  group_by(officeholder, cycle, fec_year, fec_quarter) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(officeholder, cycle) |>
  summarise(attack_count_quarter = max(n),
            first_attack_date    = min(fed_posts$attack_date[
              fed_posts$officeholder %in% cur_group()$officeholder]),
            .groups = "drop")

# Cleaner implementation
quarter_counts <- fed_posts |>
  group_by(officeholder, cycle) |>
  summarise(
    attack_count_quarter = max(tabulate(quarter(attack_date))),
    first_attack_date    = min(attack_date),
    .groups = "drop"
  )

# ── Section 2: Assemble baseline skeleton ────────────────────────────────────

cat("\n=== Section 2: Assembling cycle skeleton ===\n")

cycle_base <- baseline |>
  # Attach attack variables for treated rows; controls get NA (filled as 0 below)
  left_join(
    quarter_counts |> rename(bioname = officeholder),
    by = c("bioname", "cycle")
  ) |>
  mutate(
    attack_count_quarter = if_else(is_treated == 1L,
                                   replace_na(attack_count_quarter, 0L),
                                   NA_integer_)
  ) |>
  select(bioguide_id, bioname, state, chamber,
         cycle, is_treated, attack_count_quarter, cook_pvi_num, seniority)

cat(sprintf("Cycle skeleton: %d rows (%d treated, %d control)\n",
            nrow(cycle_base),
            sum(cycle_base$is_treated == 1L),
            sum(cycle_base$is_treated == 0L)))

# ── Section 4: Placeholders for manually-sourced variables ────────────────────
# The variables below require data from sources that cannot be automatically
# downloaded. Instructions for each are provided.

cat("=== Section 4: Manual data placeholders ===\n")

# cook_pvi_num and seniority are already in cycle_base (from baseline_panel.csv).
# Source files saved for reference:
#   cook_pvi_house.csv   — scraped from Wikipedia Cook PVI article
#   cook_pvi_senate.csv  — scraped from Wikipedia Cook PVI article
#   voteview_members.csv — Voteview HSall_members.csv (seniority computation base)

# committee_chair ──────────────────────────────────────────────────────────────
# Source: unitedstates/congress-legislators (committees.yaml)
# Coded 1 if holding full committee OR major subcommittee chair in that cycle
committee_chair <- tibble(
  bioguide_id     = NA_character_,
  cycle           = NA_integer_,
  committee_chair = NA_integer_
)
# TODO: Replace with values from congress-legislators committee data

# primary_challenger_entry, days_to_challenger_entry ──────────────────────────
# Source: FEC candidate master file (weball) — look for challengers
#   filing in the same district/state with CAND_ICI == "C" (Challenger)
# days_to_challenger_entry: days from Jan 1 of cycle year to first
#   challenger filing date (available in FEC candidate records)
challenger_data <- tibble(
  bioguide_id              = NA_character_,
  cycle                    = NA_integer_,
  primary_challenger_entry = NA_integer_,
  days_to_challenger_entry = NA_integer_
)
# TODO: Extract from weball by filtering CAND_ICI == "C" for same
#       office/state/district as each treated candidate

# challenger_quality ──────────────────────────────────────────────────────────
# Source: Ballotpedia (manual), ICPSR candidate quality data, or
#   MIT Election Data Lab candidate records
# Jacobson proxy: 1 if challenger previously held elective office
challenger_quality <- tibble(
  bioguide_id        = NA_character_,
  cycle              = NA_integer_,
  challenger_quality = NA_integer_
)
# TODO: Code manually from Ballotpedia or cross-reference with
#       VoteView / ICPSR legislative career data

# incumbent_retired ────────────────────────────────────────────────────────────
# Source: FEC weball — if incumbent has no filing in next cycle with
#   CAND_ICI == "I", they did not seek re-election.
#   Cross-check against Congress biographical retirement data.
retirement_data <- tibble(
  bioguide_id       = NA_character_,
  cycle             = NA_integer_,
  incumbent_retired = NA_integer_
)

# primary_victory ──────────────────────────────────────────────────────────────
# Source: MIT Election Data Lab (https://electionlab.mit.edu/data)
#   "U.S. House 1976–2024" and "U.S. Senate 1976–2024" datasets
#   Filter to primary elections; match on state, district, cycle, candidate name.
primary_results <- tibble(
  bioguide_id     = NA_character_,
  cycle           = NA_integer_,
  primary_victory = NA_integer_
)

# ── Section 5: Join everything and write output ───────────────────────────────

cat("=== Section 5: Assembling final cycle dataset ===\n")

cycle_dataset <- cycle_base |>
  left_join(committee_chair,   by = c("bioguide_id", "cycle")) |>
  left_join(challenger_data,   by = c("bioguide_id", "cycle")) |>
  left_join(challenger_quality, by = c("bioguide_id", "cycle")) |>
  left_join(retirement_data,   by = c("bioguide_id", "cycle")) |>
  left_join(primary_results,   by = c("bioguide_id", "cycle")) |>
  select(
    bioguide_id,
    bioname,
    state,
    chamber,
    cycle,
    is_treated,
    attack_count_quarter,
    cook_pvi_num,
    seniority,
    committee_chair,
    primary_challenger_entry,
    challenger_quality,
    days_to_challenger_entry,
    incumbent_retired,
    primary_victory
  ) |>
  arrange(desc(is_treated), bioname, cycle)

write_csv(cycle_dataset, OUT_FILE)

cat(sprintf("\nCycle dataset: %d rows × %d cols → %s\n",
            nrow(cycle_dataset), ncol(cycle_dataset), OUT_FILE))
cat("\nColumns with all-NA (need manual data):\n")
cycle_dataset |>
  summarise(across(everything(), ~all(is.na(.)))) |>
  pivot_longer(everything()) |>
  filter(value) |>
  pull(name) |>
  cat(sep = "\n  ")
