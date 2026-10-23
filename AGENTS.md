# RINO Research Project — Memory File
*Last updated: August 21, 2026*

## Project Overview
Political science research on the effect of Donald Trump labeling Republican officeholders as "RINOs." Building two datasets for causal inference:
1. **RDiT dataset** — daily fundraising panel (±30 days around each RINO attack)
2. **Cycle dataset** — cycle-level panel for logit and survival models

A completed EDA report (`rino_eda.qmd`) covers attack event patterns and electoral consequences for federal officeholders through 2026.

## Key Data Files

| File | Description |
|---|---|
| `rino_data.xlsx` | Google Sheets export with 6 tabs: `all_events` (331 rows × 7 cols), `rino_events` (253 rows — event × RINO target pairs), `rino_officeholders` (58 rows), `rinos_by_cycle` (145 rows × 7 cols), `people_name_id` (119 rows), `people_labels` (616 rows × 8 cols). Load with `readxl::read_excel()`. |
| `rino_posts.csv` | 156 rows, 137 unique posts, 2020-03-25 to 2026-06-08. One row per post × officeholder. Columns: `post_id`, `officeholder`, `reposts`, `likes`, `date_time`, `platform`, `source`. Note: `likes` missing for 42 posts from Jan 2026 onward; p046 (Brian Kemp, 2023-09-03) has `likes = 19.5` (likely 19500). |
| `rino_officeholders.csv` | 63 total RINO-labeled officeholders |
| `rino_federal_officeholders.csv` | 26 federal treated officials (House + Senate only; presidential candidates excluded). Does **not** have a `person_id` column — use `rino_data.xlsx` `rino_officeholders` tab for person_id mapping. |
| `rino_state_officeholders.csv` | 32 state-level officials (governors, state legislators, SoS, etc.) |
| `rino_officeholder_metadata.csv` | Office, state, chamber, party, FEC search name for all 63 |
| `baseline_panel.csv` | 1,110 rows: 82 treated rows (26 officials × cycles) + 1,028 control rows. Columns: `bioguide_id`, `bioname`, `state`, `chamber`, `cycle`, `is_treated`, `cook_pvi_num`, `seniority`. |
| `cem_matched_data.csv` | Output of CEM matching. 66 treated rows + 783 control rows (310 unique control officials). Includes `subclass` and `weights`. Tom Emmer (PVI=10 on bin boundary) is the only unmatched treated official. |
| `control_cmte_map.csv` | Maps matched control `bioguide_id` → FEC `committee_id`. 302 of 310 controls mapped (97%). |
| `fec_candidates.csv` | FEC candidate IDs for 25 of 26 treated officials (Joe O'Dea unmatched). |
| `fec_cycle_totals.csv` | Cycle-level FEC fundraising totals per treated candidate |
| `fec_quarterly.csv` | Quarterly FEC filing data (cycle-level, not truly quarterly) |
| `cook_pvi_house.csv` | Cook PVI by House district (scraped from Wikipedia) |
| `cook_pvi_senate.csv` | Cook PVI by state for Senate (scraped from Wikipedia) |
| `voteview_members.csv` | Full Voteview HSall_members.csv — used for seniority computation. Cycle mapping: `cycle = congress * 2 + 1786` (Congress 117 → 2020, 118 → 2022, 119 → 2024). Covers through Congress 119 (2024); Congress 120 (2026) not yet available. |
| `cycle_dataset.csv` | 1,110-row cycle panel. Base dataset — do not use alone; use `cycle_panel_with_outcomes.csv` for analysis. |
| `cycle_panel_with_outcomes.csv` | **Primary analysis file.** 1,110 rows × 23 cols. `cycle_dataset.csv` merged with `rino_outcome_skeleton.csv`. Adds: `is_election_year`, `primary_opponent_vote_share`, `substantial_challenge`, `outcome`, `lost_primary`, `retired_post_attack`, `cumulative_attacks`, `cumulative_attacks_corrected`. Load with `read_csv()`. See *Corrected Attack Variable* section below. |
| `rino_outcome_skeleton.csv` | Per-official × cycle outcome coding for 26 treated federal officials. All 2026 primaries resolved as of Aug 15, 2026. See Electoral Outcomes section below. |
| `rdit_dataset.csv` | **Current build**: 72,163 rows × 16 cols. `daily_small_dollar_receipts` now populated (WinRed bug fixed and re-run Aug 21). |

## Scripts and Reports

| File | Purpose | Status |
|---|---|---|
| `rino_eda.qmd` | Full EDA + descriptive outcomes + logit models + alluvial diagram | **Complete** ✓ — renders to HTML and PDF |
| `collect_fec_data.R` | Downloads FEC weball + ccl bulk files; resolves candidate IDs; produces cycle totals | Working ✓ |
| `build_rdit_dataset.R` | Builds full treated + control daily fundraising panel | Complete ✓ — re-run Aug 21; `daily_small_dollar_receipts` now populated |
| `build_cycle_dataset.R` | Builds cycle-level panel for logit/survival | Partial — superseded by `cycle_panel_with_outcomes.csv` |
| `rdit_models.R` | RDiT regression models (fixest::feols) | In progress |

## rino_data.xlsx — Event Dataset Key Facts

- **87 unique individuals** labeled RINO in coded events (from `rino_events` tab)
- **58 officeholders** / **29 non-officeholders** (former officials, donors, media figures — e.g., Paul Ryan 43 events, Karl Rove 14)
- **26 federal** / **32 state** officeholders among the 58
- Platform shift: Twitter/X dominant through Jan 2021 (ban) → Truth Social dominant from mid-2022 onward
- Attack volume rises during primary seasons (Mar–Aug in election years), troughs after general elections
- Q1 2026 is the highest single quarter on record; may reflect shift to governing tool rather than electoral one
- `all_events` has 331 rows; `rino_events` has 253 (some events have no coded RINO target; some events target multiple people)

## Electoral Outcomes for Treated Federal Officials

**Substantial challenge** defined as top primary opponent receiving ≥ 25% of the vote.
**Outcomes** (worst per official, N=23 with electoral data; O'Dea, Hogan, Daughtry excluded — no federal committee):

| Outcome | N | Officials |
|---|---|---|
| Won primary (all cycles) | 7 | Collins, Thune, Young, Emmer, Ricketts, Murkowski, Hurd |
| Retired before election | 11 | Sasse, Toomey, Burr, Romney, Ken Buck, Fred Upton, McConnell, Newhouse (Dec 2025), Mace (ran for SC governor), Chip Roy (ran for TX AG), MTG (resigned 2026) |
| Lost primary | 5 | Liz Cheney 2022 (Hageman 66.9%), Peter Meijer 2022 (Gibbs 52.9%), Thomas Massie 2026 (Gallrein 54.4%), Bill Cassidy 2026 (Letlow 44.8%), John Cornyn 2026 (Paxton 63.8% in runoff) |

**Key pattern:** All 5 primary losses and 2 of 11 retirements came from officials who faced a substantial challenge (≥ 25%). No official who never faced a substantial challenge lost their primary (0 of 14).

## Logit Model Results (from rino_eda.qmd)

Exposure: `cumulative_attacks_corrected` (running sum of pre-primary attack counts per official through each cycle). Two models on treated officials in election years. Original (uncorrected) results shown for reference.

| Model | N | OR (original) | OR (corrected) | p (original) | p (corrected) |
|---|---|---|---|---|---|
| Substantial challenge (≥ 25%) | 33 | 2.53 | 3.13 | 0.091 | 0.079 |
| Lost primary | 48 | 2.32 | 2.54 | 0.061 | 0.044 |

Cook PVI, seniority, and chamber show no meaningful association in either model. **Both models are underpowered** (5 primary losses across 48 rows); treat as exploratory/hypothesis-generating only. The corrected lost-primary model crosses p < 0.05; interpret cautiously given sample size.

## rino_outcome_skeleton.csv — Variable Coverage

| Variable | Coverage | Notes |
|---|---|---|
| `outcome` | 23/23 named officials | `won_primary`, `lost_primary`, `retired`; 3 officials excluded (no bioguide_id) |
| `primary_opponent_vote_share` | ~25/33 coded election years | NAs for Massie 2022/2024 (confirmed < 25% per Massie's own statement), Pete Ricketts 2026, Susan Collins 2026 |
| `substantial_challenge` | 33/48 election-year rows | Derived: `primary_opponent_vote_share >= 0.25`; directly coded FALSE for Massie 2022/2024 |
| `is_election_year` | 100% | House: always TRUE; Senate: derived from class lookup |
| `cumulative_attacks` | 100% | Running cumsum of `attack_count_quarter` per official |
| `cumulative_attacks_corrected` | 100% (treated rows); NA for controls | Same as above but 4 cycle-rows corrected to exclude post-primary attacks. Use this variable in models. |

## build_rdit_dataset.R — Key Details

**Prerequisites:** `cem_matched_data.csv`, `control_cmte_map.csv`, `rino_posts.csv`, `rino_federal_officeholders.csv`, `fec_bulk/` directory.

**Run as a background job** (Tools → Background Jobs) — takes 30–90 min.

**Key design decisions:**
- `daily_small_dollar_receipts`: WinRed conduit PAC (C00694323) earmarks read from recipient committee's Schedule A (`CMTE_ID ∈ target_cmtes` AND `OTHER_ID == "C00694323"` AND `MEMO_CD == "X"`). WinRed bug fixed Jun 25.
- Hogan, Ricketts, O'Dea: no federal committee during attack window → `daily_total_receipts = NA`

## Matching Design (CEM)

- `chamber`: exact match; `cook_pvi_num`: 4 bins; `cycle`: exact; `seniority`: 3 tiers
- Result: 22 treated matched; 310 controls; Tom Emmer unmatched (PVI boundary)

## Corrected Attack Variable (`cumulative_attacks_corrected`)

Four official × cycle rows in the federal panel had `attack_count_quarter` inflated by attacks that occurred *after* the cycle's primary date. These were corrected before reaccumulating the running sum:

| Official | Cycle | Primary date | Issue |
|---|---|---|---|
| Ben Sasse | 2020 | May 12, 2020 | Both 2020 attacks (Aug 10, Oct 17) post-primary; aq_corrected = 0 |
| Lisa Murkowski | 2022 | Aug 16, 2022 | Only attack (Aug 30) 14 days post-primary; aq_corrected = 0 |
| Todd Young | 2022 | May 3, 2022 | Only attack (Oct 17) post-primary; aq_corrected = 0 |
| Dan Newhouse | 2024 | Aug 6, 2024 | Q3 had 2 attacks (Aug 3 pre, Aug 14 post); aq_corrected = 1 |

**Key Murkowski finding:** Murkowski 2022 is a `substantial_challenge = TRUE` case with `cumulative_attacks = 1`, but that 1 attack came 14 days after her primary. Correctly, she had 0 pre-primary RINO attacks before her substantial challenge. The original model treated her as evidence of attack → challenge, but the ordering was reversed.

The bias from these contaminated rows was downward (more FALSE cases were overcounted than TRUE), so corrected estimates are modestly larger than originals.

## Known Data Issues

- `rino_posts.csv` p046: `likes = 19.5` should be ~19500
- Joe O'Dea, Larry Hogan, Kelly Daughtry: `bioguide_id = NA` — excluded from logit models
- `cycle_dataset.csv` `primary_challenger_entry`: Senate rows in non-election cycles are unreliable (state-level bug, not per-senator)
- `primary_victory` in cycle_dataset: Voteview proxy, does not distinguish primary vs. general loss; superseded by `outcome` in `cycle_panel_with_outcomes.csv`
- **Rusty Bowers (state combined model):** His only RINO attack (Aug 3, 2022) came one day *after* his August 2, 2022 AZ State Senate primary loss. In the combined Firth logit he contributes `lost_primary = TRUE, cumulative_attacks = 1` with reversed temporal ordering. The combined model chunks have `eval: false`; correction pending.
- **Roger Roth (state):** Last of 3 attacks (Aug 14, 2024) came one day after his WI Senate primary (Aug 13). He retired. Lower priority than Bowers since outcome is not primary loss.

## Remaining To-Do

**Highest priority (RDiT):**
1. ~~Re-run `build_rdit_dataset.R`~~ ✓ Done Aug 21
2. Run RDiT models (`rdit_models.R`) — small-dollar data now available

**Cycle dataset / logit improvements:**
- `committee_chair`: Congress.gov API or congress-legislators (blocked in-session; download externally)
- `challenger_quality` (Jacobson proxy): Ballotpedia manual coding feasible for 26 treated officials
- `days_to_challenger_entry`: FEC CN bulk files (`https://www.fec.gov/files/bulk-downloads/{YEAR}/cn{YY}.zip`)
- Fill remaining ~8 missing `primary_opponent_vote_share` values (Massie 2022/2024 from KY SOS; Ricketts 2024/2026 from NE SOS; etc.)
- Fix Senate class bug in `primary_challenger_entry`
- Consider lagged specification: attacks through cycle T−1 predicting outcomes in cycle T

**Broader design:**
- State-level officials (32) not yet addressed electorally — FollowTheMoney.org for campaign finance
- `attack_venue` column: other venue types (Press Release, Live Rally, Endorsement of opponent) not yet coded

## Data Sources

| Source | Used for | Access |
|---|---|---|
| `rino_data.xlsx` | Event-level attack data, officeholder lists, people metadata | Local file — originally from Google Sheets (private; `gs4_auth()` required in interactive session) |
| FEC bulk downloads (`fec.gov/files/bulk-downloads/`) | Candidate IDs, committee IDs, cycle totals, Schedule A daily transactions | Free, no key |
| WinRed PAC FEC filings (C00694323) | Small-dollar receipts via Schedule A memo items | Via FEC indiv bulk files |
| Voteview (`voteview.com/static/data/out/members/HSall_members.csv`) | Seniority, congressional membership, district codes | Free |
| Wikipedia Cook PVI tables | District/state partisan lean | Free, scraped via rvest |
| Ballotpedia | Primary results, vote shares, candidate histories | Web search / manual scraping |
| FEC API (`api.fec.gov`) | Not accessible from RStudio session — use bulk files instead | DNS blocked in session |
| Congress.gov API (`api.congress.gov`) | Committee chair data | Free with key; blocked in-session |
