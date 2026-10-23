library(rvest)
library(tidyverse)

# Output files (scraper saves progress so it can be resumed if interrupted)
INDEX_FILE <- "trump_email_index.csv"
RINO_FILE  <- "trump_rino_emails.csv"

# Delay between HTTP requests (seconds) — be a polite guest
REQUEST_DELAY <- 0.5

# ── Helpers ───────────────────────────────────────────────────────────────────

scrape_index_page <- function(url) {
  page <- read_html(url)

  anchors <- page |> html_elements("a")
  hrefs   <- anchors |> html_attr("href")

  msg_idx      <- grepl("https://politicalemails.org/messages/\\d+$", hrefs)
  msg_urls     <- hrefs[msg_idx]
  msg_ids      <- as.integer(str_extract(msg_urls, "\\d+$"))
  msg_subjects <- anchors[msg_idx] |> html_element("h3")   |> html_text2()
  msg_senders  <- anchors[msg_idx] |> html_element("strong") |> html_text2()
  msg_full     <- anchors[msg_idx] |> html_text2()
  msg_dates    <- str_extract(msg_full, "[A-Z][a-z]+ \\d+, \\d{4} at \\d+:\\d+ [AP]M")

  next_link <- page |>
    html_elements("a") |>
    keep(~ grepl("Next", html_text2(.x))) |>
    html_attr("href")

  next_url <- if (length(next_link) > 0 && !is.na(next_link[1])) {
    paste0("https://politicalemails.org", next_link[1])
  } else {
    NA_character_
  }

  list(
    msgs = tibble(
      message_id = msg_ids,
      url        = msg_urls,
      sender     = msg_senders,
      subject    = msg_subjects,
      date_str   = msg_dates
    ),
    next_url = next_url
  )
}

fetch_email_body <- function(message_id) {
  body_url  <- paste0("https://politicalemails.org/messages/", message_id, "/html_body")
  body_page <- tryCatch(read_html(body_url), error = function(e) NULL)
  if (is.null(body_page)) return(NA_character_)
  body_page |> html_element("body") |> html_text2()
}

# ── Phase 1: Collect index of all messages ────────────────────────────────────

cat("=== Phase 1: Collecting message index ===\n")

all_msgs <- list()
page_num <- 1L
next_url <- "https://politicalemails.org/organizations/415"

# Resume from saved index if it exists
if (file.exists(INDEX_FILE)) {
  cat("Found existing index file — loading and resuming from last cursor.\n")
  all_msgs_df <- read_csv(INDEX_FILE, show_col_types = FALSE)
  cat(sprintf("Loaded %d messages from index.\n", nrow(all_msgs_df)))
} else {
  repeat {
    cat(sprintf("  Fetching index page %d: %s\n", page_num,
                substr(next_url, 1, 80)))

    result   <- tryCatch(scrape_index_page(next_url), error = function(e) {
      message("  ERROR on page ", page_num, ": ", conditionMessage(e))
      NULL
    })

    if (is.null(result)) break

    all_msgs[[page_num]] <- result$msgs
    cat(sprintf("  → %d messages collected (total so far: %d)\n",
                nrow(result$msgs), sum(map_int(all_msgs, nrow))))

    if (is.na(result$next_url)) {
      cat("  No next page — index collection complete.\n")
      break
    }

    next_url <- result$next_url
    page_num <- page_num + 1L
    Sys.sleep(REQUEST_DELAY)
  }

  all_msgs_df <- bind_rows(all_msgs) |> distinct(message_id, .keep_all = TRUE)
  write_csv(all_msgs_df, INDEX_FILE)
  cat(sprintf("\nIndex saved: %d unique messages → %s\n\n",
              nrow(all_msgs_df), INDEX_FILE))
}

# ── Phase 2: Fetch email bodies and search for RINO ───────────────────────────

cat("=== Phase 2: Fetching email bodies (searching for 'RINO') ===\n")

# Resume: skip message IDs already written to the RINO file
already_checked <- integer(0)
if (file.exists(RINO_FILE)) {
  checked_df       <- read_csv(RINO_FILE, show_col_types = FALSE)
  already_checked  <- checked_df$message_id
  cat(sprintf("Resuming — %d messages already processed, %d RINO hits found.\n",
              length(already_checked), nrow(checked_df[checked_df$contains_rino, ])))
}

# Also track all-checked IDs in a separate lightweight file
checked_ids_file <- "trump_emails_checked_ids.rds"
if (file.exists(checked_ids_file)) {
  already_checked <- readRDS(checked_ids_file)
}

ids_to_check <- setdiff(all_msgs_df$message_id, already_checked)
cat(sprintf("%d messages to check.\n\n", length(ids_to_check)))

rino_hits    <- list()
checked_ids  <- already_checked

for (i in seq_along(ids_to_check)) {
  mid <- ids_to_check[i]

  if (i %% 50 == 0 || i == length(ids_to_check)) {
    cat(sprintf("  [%d / %d] message_id %d\n", i, length(ids_to_check), mid))
  }

  body <- fetch_email_body(mid)
  checked_ids <- c(checked_ids, mid)

  if (!is.na(body) && str_detect(body, regex("\\bRINO\\b", ignore_case = FALSE))) {
    meta <- filter(all_msgs_df, message_id == mid)

    # Extract surrounding context (up to 300 chars around each RINO mention)
    matches <- str_extract_all(body, ".{0,150}\\bRINO\\b.{0,150}")[[1]]

    rino_hits[[length(rino_hits) + 1]] <- tibble(
      message_id  = mid,
      date_str    = meta$date_str,
      sender      = meta$sender,
      subject     = meta$subject,
      url         = meta$url,
      n_mentions  = length(matches),
      context     = paste(matches, collapse = " ||| ")
    )

    cat(sprintf("  *** RINO found in message %d (%s) — %d mention(s)\n",
                mid, meta$date_str, length(matches)))
  }

  # Save progress every 100 messages
  if (i %% 100 == 0 || i == length(ids_to_check)) {
    saveRDS(checked_ids, checked_ids_file)
    if (length(rino_hits) > 0) {
      rino_df <- bind_rows(rino_hits)
      write_csv(rino_df, RINO_FILE)
    }
  }

  Sys.sleep(REQUEST_DELAY)
}

# ── Final output ──────────────────────────────────────────────────────────────

if (length(rino_hits) > 0) {
  rino_df <- bind_rows(rino_hits)
  write_csv(rino_df, RINO_FILE)
  cat(sprintf("\n=== Done! ===\n%d emails containing 'RINO' saved to: %s\n",
              nrow(rino_df), RINO_FILE))
  print(rino_df |> select(date_str, sender, subject, n_mentions))
} else {
  cat("\n=== Done! No emails containing 'RINO' found. ===\n")
}
