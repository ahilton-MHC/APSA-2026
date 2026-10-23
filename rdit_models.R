# Formal RDiT models

library(fixest)
library(tidyverse)

# Data prep — filter to matched, non-NA observations
df <- rdit |>
  filter(!is.na(daily_total_receipts), !is.na(subclass)) |>
  mutate(
    log_total = log1p(daily_total_receipts),
    log_small = log1p(daily_small_dollar_receipts),
    is_treated = as.integer(is_treated),
    post_attack = as.integer(post_attack)
  )

cat("Analysis sample:\n")
df |>
  group_by(is_treated) |>
  summarise(
    n_rows = n(),
    n_officials = n_distinct(officeholder),
    pct_zero = round(mean(daily_total_receipts == 0) * 100, 1),
    median_receipts = median(daily_total_receipts)
  ) |>
  print()


# Event study plot — mean log(1 + receipts) by day, treated vs. control
df |>
  group_by(days_from_attack, is_treated) |>
  summarise(mean_log = mean(log_total), .groups = "drop") |>
  mutate(Group = if_else(is_treated == 1, "Treated (RINO-labeled)", "Matched Controls")) |>
  ggplot(aes(x = days_from_attack, y = mean_log, color = Group)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Treated (RINO-labeled)" = "#C0392B", "Matched Controls" = "#2980B9")) +
  labs(
    title = "Daily Fundraising Around RINO Attack Date",
    x = "Days from Attack (0 = attack date)",
    y = "Mean log(1 + daily receipts)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# Specification: DiD at the discontinuity, with subclass FE to absorb matched-group differences
# Clustering on officeholder (note: only ~21 treated clusters — SEs should be interpreted cautiously)

# Model 1: Total receipts — simple post × treated jump
m1 <- feols(log_total ~ post_attack * is_treated + days_from_attack |
              subclass,
            data = df,
            weights = ~weights,
            cluster = ~officeholder)

# Model 2: Total receipts — allow different slope before/after
m2 <- feols(log_total ~ post_attack * is_treated + days_from_attack * post_attack |
              subclass,
            data = df,
            weights = ~weights,
            cluster = ~officeholder)

# Model 3: Small dollar receipts
m3 <- feols(log_small ~ post_attack * is_treated + days_from_attack * post_attack |
              subclass,
            data = df,
            weights = ~weights,
            cluster = ~officeholder)

etable(m1, m2, m3,
       keep = c("post_attack", "is_treated", "post_attack:is_treated"),
       headers = c("Total (simple)", "Total (trends)", "Small dollar"),
       digits = 3)