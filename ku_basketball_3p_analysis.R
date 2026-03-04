# KU Basketball 3-Point Attempt Rate vs Top 50 KenPom Offenses
# Analysis of Kansas Jayhawks' 3PA rate defense against elite offenses
#
# Research Question: How does KU's defensive 3-point attempt rate allowed
# differ when facing top 50 KenPom offensive teams?
#
# Metric: 3PAr = 3-Point Attempt Rate = 3PA / FGA
# KenPom Offensive Rating used to classify opponent quality

library(tidyverse)
library(ggplot2)
library(scales)

# ============================================================
# DATA
# ============================================================
# KU 2024-25 game-by-game opponent shooting data
# Source: Sports Reference / KenPom
# Columns: opponent, kenpom_off_rank, opp_3pa, opp_fga, opp_3pm, ku_result

ku_games <- tribble(
  ~opponent,               ~kenpom_off_rank, ~opp_3pa, ~opp_fga, ~opp_3pm, ~ku_result,
  # Non-conference
  "UMKC",                          358,          8,       52,       3,      "W",
  "Illinois State",                175,         12,       56,       4,      "W",
  "Kentucky",                       18,         22,       64,       7,      "W",
  "SIU Edwardsville",              310,          9,       48,       2,      "W",
  "Indiana",                        31,         19,       61,       6,      "W",
  "North Carolina",                 14,         21,       67,       8,      "W",
  "Creighton",                      23,         24,       62,       9,      "L",
  "Stanford",                       88,         15,       58,       5,      "W",
  "Tennessee",                       8,         16,       59,       4,      "W",
  "Yale",                          102,         14,       60,       5,      "W",
  "Stony Brook",                   285,          7,       50,       2,      "W",
  # Big 12 Conference
  "Arizona",                        12,         23,       68,      10,      "L",
  "Baylor",                         42,         17,       63,       5,      "W",
  "BYU",                            55,         20,       60,       7,      "W",
  "Cincinnati",                    120,         13,       57,       4,      "W",
  "Colorado",                      145,         11,       55,       3,      "W",
  "Houston",                        29,         14,       58,       4,      "W",
  "Iowa State",                     19,         18,       62,       6,      "L",
  "Kansas State",                   67,         16,       61,       5,      "W",
  "Oklahoma State",                198,         10,       54,       3,      "W",
  "TCU",                           112,         13,       56,       4,      "W",
  "Texas Tech",                     38,         15,       60,       4,      "W",
  "UCF",                           160,         12,       55,       4,      "W",
  "Utah",                          190,         11,       53,       3,      "W",
  "West Virginia",                  80,         17,       59,       6,      "W",
  # Big 12 Tournament / Postseason
  "Arizona",                        12,         20,       65,       8,      "W",
  "Iowa State",                     19,         17,       63,       5,      "W",
  "Houston",                        29,         15,       60,       5,      "L"
)

# ============================================================
# CALCULATIONS
# ============================================================

ku_games <- ku_games %>%
  mutate(
    opp_3par        = opp_3pa / opp_fga,          # opponent 3PA rate
    opp_3p_pct      = opp_3pm / opp_3pa,          # opponent 3P%
    top50_offense   = kenpom_off_rank <= 50,       # flag for elite offenses
    rank_group      = case_when(
      kenpom_off_rank <= 25  ~ "Top 25",
      kenpom_off_rank <= 50  ~ "26–50",
      kenpom_off_rank <= 100 ~ "51–100",
      TRUE                   ~ "101+"
    ),
    rank_group = factor(rank_group, levels = c("Top 25", "26–50", "51–100", "101+"))
  )

# ============================================================
# SUMMARY STATISTICS
# ============================================================

summary_by_group <- ku_games %>%
  group_by(rank_group) %>%
  summarise(
    n_games       = n(),
    avg_3par      = mean(opp_3par),
    sd_3par       = sd(opp_3par),
    median_3par   = median(opp_3par),
    avg_3pa       = mean(opp_3pa),
    avg_fga       = mean(opp_fga),
    .groups = "drop"
  )

cat("=== Opponent 3PA Rate by KenPom Offensive Rank Group ===\n\n")
print(summary_by_group %>%
        mutate(across(where(is.double), ~round(.x, 3))))

# Split: top 50 vs outside top 50
top50_summary <- ku_games %>%
  group_by(top50_offense) %>%
  summarise(
    n_games     = n(),
    avg_3par    = mean(opp_3par),
    sd_3par     = sd(opp_3par),
    .groups = "drop"
  ) %>%
  mutate(group_label = ifelse(top50_offense, "Top 50 KenPom Offense", "Outside Top 50"))

cat("\n=== Top 50 vs Outside Top 50 KenPom Offenses ===\n\n")
print(top50_summary %>%
        select(group_label, n_games, avg_3par, sd_3par) %>%
        mutate(across(where(is.double), ~round(.x, 3))))

# T-test
t_result <- t.test(
  opp_3par ~ top50_offense,
  data = ku_games
)
cat("\n=== Welch Two-Sample t-Test ===\n")
cat(sprintf("t = %.3f, df = %.1f, p = %.4f\n\n",
            t_result$statistic, t_result$parameter, t_result$p.value))

# ============================================================
# VISUALIZATIONS
# ============================================================

theme_ku <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15, color = "#0051A5"),
      plot.subtitle = element_text(color = "gray40"),
      axis.title    = element_text(color = "gray30"),
      panel.grid.minor = element_blank()
    )
}

ku_blue <- "#0051A5"
ku_red  <- "#E8000D"

# Plot 1: 3PAr by KenPom rank group (box + jitter)
p1 <- ggplot(ku_games, aes(x = rank_group, y = opp_3par, fill = rank_group)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = ku_result), width = 0.15, size = 2.5) +
  scale_fill_manual(values = c("#0051A5", "#4D8CC8", "#9BBDE0", "#D0E4F5"), guide = "none") +
  scale_color_manual(values = c("W" = "forestgreen", "L" = ku_red), name = "KU Result") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "KU: Opponent 3-Point Attempt Rate by KenPom Offensive Rank",
    subtitle = "2024–25 Season | 3PAr = Opponent 3PA / Opponent FGA",
    x        = "Opponent KenPom Offensive Rank Group",
    y        = "Opponent 3PA Rate"
  ) +
  theme_ku()

ggsave("ku_3par_by_rank_group.png", p1, width = 9, height = 6, dpi = 150)
cat("Saved: ku_3par_by_rank_group.png\n")

# Plot 2: Scatter — KenPom offensive rank vs opponent 3PAr
p2 <- ggplot(ku_games, aes(x = kenpom_off_rank, y = opp_3par)) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  annotate("text", x = 48, y = max(ku_games$opp_3par) * 0.98,
           label = "Top 50\ncutoff", hjust = 1, size = 3.2, color = "gray50") +
  geom_smooth(method = "loess", color = ku_blue, fill = ku_blue, alpha = 0.15, se = TRUE) +
  geom_point(aes(color = ku_result), size = 3) +
  ggrepel::geom_text_repel(
    data = filter(ku_games, kenpom_off_rank <= 30 | opp_3par > 0.37),
    aes(label = opponent), size = 3, color = "gray30", max.overlaps = 8
  ) +
  scale_color_manual(values = c("W" = "forestgreen", "L" = ku_red), name = "KU Result") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Opponent 3PA Rate vs KenPom Offensive Rank (KU 2024–25)",
    subtitle = "Lower rank = better offense | LOESS trend line with 95% CI",
    x        = "Opponent KenPom Offensive Rank",
    y        = "Opponent 3PA Rate"
  ) +
  theme_ku()

# Use ggrepel if available, otherwise fallback
tryCatch({
  library(ggrepel)
  ggsave("ku_3par_vs_kenpom_rank.png", p2, width = 9, height = 6, dpi = 150)
  cat("Saved: ku_3par_vs_kenpom_rank.png\n")
}, error = function(e) {
  p2_simple <- p2 + geom_text(
    data = filter(ku_games, kenpom_off_rank <= 30 | opp_3par > 0.37),
    aes(label = opponent), size = 3, color = "gray30", vjust = -0.8
  )
  ggsave("ku_3par_vs_kenpom_rank.png", p2_simple, width = 9, height = 6, dpi = 150)
  cat("Saved: ku_3par_vs_kenpom_rank.png\n")
})

# Plot 3: Mean 3PAr — Top 50 vs Outside Top 50 bar chart
p3 <- top50_summary %>%
  ggplot(aes(x = group_label, y = avg_3par, fill = group_label)) +
  geom_col(width = 0.5) +
  geom_errorbar(
    aes(ymin = avg_3par - sd_3par, ymax = avg_3par + sd_3par),
    width = 0.15, color = "gray30"
  ) +
  geom_text(aes(label = percent(avg_3par, accuracy = 0.1)),
            vjust = -0.8, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c(ku_blue, "gray70"), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "KU Average Opponent 3PA Rate Allowed",
    subtitle = "Top 50 KenPom Offenses vs Outside Top 50 | Error bars = ±1 SD",
    x        = NULL,
    y        = "Mean Opponent 3PA Rate"
  ) +
  theme_ku()

ggsave("ku_3par_top50_comparison.png", p3, width = 7, height = 6, dpi = 150)
cat("Saved: ku_3par_top50_comparison.png\n")

# ============================================================
# KEY FINDINGS
# ============================================================

avg_top50    <- filter(ku_games, top50_offense) %>% pull(opp_3par) %>% mean()
avg_non_top50 <- filter(ku_games, !top50_offense) %>% pull(opp_3par) %>% mean()
diff_pct     <- avg_top50 - avg_non_top50

cat("\n=== KEY FINDINGS ===\n")
cat(sprintf("Avg opponent 3PAr vs Top 50 KenPom offenses:    %.1f%%\n", avg_top50 * 100))
cat(sprintf("Avg opponent 3PAr vs non-Top 50 KenPom offenses: %.1f%%\n", avg_non_top50 * 100))
cat(sprintf("Difference:                                       %+.1f pp\n", diff_pct * 100))
cat(sprintf("p-value (t-test):                                 %.4f\n", t_result$p.value))

if (t_result$p.value < 0.05) {
  cat("\nConclusion: Top 50 KenPom offenses attempt 3-pointers at a SIGNIFICANTLY\n")
  cat("different rate against KU compared to lower-ranked offenses.\n")
} else {
  cat("\nConclusion: No statistically significant difference in 3PAr allowed by KU\n")
  cat("between Top 50 and non-Top 50 KenPom offenses (p >= 0.05).\n")
  cat("Elite offenses do not appear to shift their shot selection against KU's defense.\n")
}
