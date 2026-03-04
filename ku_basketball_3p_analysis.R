# KU Basketball: 3P% vs Offensive Efficiency Among Top 50 T-Rank Offenses
# 2025-26 Season — Real data via cbbdata (barttorvik.com / T-Rank)
#
# Questions:
#   1. What is the general relationship between team 3P% and offensive efficiency?
#   2. How does KU's 3P% compare to other top 50 T-Rank offensive teams?
#
# T-Rank (barttorvik.com) is a free KenPom-equivalent metric. adj_o = adjusted
# offensive efficiency (points per 100 possessions, schedule-adjusted).

# ── Install once (uncomment and run if needed) ────────────────────────────────
# install.packages("devtools")
# devtools::install_github("andreweatherman/cbbdata")
# install.packages(c("tidyverse", "ggplot2", "scales", "ggrepel"))
# ─────────────────────────────────────────────────────────────────────────────

library(cbbdata)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)

# ============================================================
# PULL REAL DATA
# ============================================================

ratings <- cbd_torvik_ratings(year = 2026)

# Uncomment to inspect available columns if something errors below:
# names(ratings)
# glimpse(ratings)

# ============================================================
# FILTER: Top 50 by offensive efficiency
# ============================================================

top50 <- ratings %>%
  arrange(desc(adj_o)) %>%
  slice_head(n = 50) %>%
  mutate(
    off_rank = row_number(),
    is_ku    = str_detect(team, regex("^kansas$", ignore_case = TRUE))
  )

# ============================================================
# ANALYSIS
# ============================================================

# 1. Pearson correlation: 3P% vs adjusted offensive efficiency
cor_test <- cor.test(top50$off_three_pct, top50$adj_o, method = "pearson")

# 2. Linear model
lm_fit <- lm(adj_o ~ off_three_pct, data = top50)

# 3. KU-specific stats
ku_row <- filter(top50, is_ku)

if (nrow(ku_row) == 0) {
  warning("Kansas not found in top 50. Check team name with: filter(ratings, str_detect(team, 'Kansas'))")
}

ku_3pct_rank <- top50 %>%
  arrange(desc(off_three_pct)) %>%
  mutate(rank_3pct = row_number()) %>%
  filter(is_ku) %>%
  pull(rank_3pct)

# ============================================================
# PRINT RESULTS
# ============================================================

cat("=== 3P% vs Adjusted Offensive Efficiency (Top 50 T-Rank Offenses, 2025-26) ===\n\n")
cat(sprintf("Pearson r  = %.3f\n", cor_test$estimate))
cat(sprintf("p-value    = %.4f\n\n", cor_test$p.value))

cat("=== Linear Model: Adj-O ~ 3P% ===\n")
print(summary(lm_fit))

if (nrow(ku_row) > 0) {
  cat("\n=== Kansas Among Top 50 T-Rank Offenses ===\n")
  cat(sprintf("KU Adj-O rank (offense):    #%d of 50\n", ku_row$off_rank))
  cat(sprintf("KU 3P%%:                     %.1f%%\n", ku_row$off_three_pct * 100))
  cat(sprintf("KU 3P%% rank (in top 50):    %d of 50\n", ku_3pct_rank))
  cat(sprintf("Group avg 3P%%:              %.1f%%\n", mean(top50$off_three_pct) * 100))
  cat(sprintf("KU vs group avg:            %+.1f pp\n",
              (ku_row$off_three_pct - mean(top50$off_three_pct)) * 100))
}

# ============================================================
# VISUALIZATIONS
# ============================================================

ku_blue <- "#0051A5"
ku_red  <- "#E8000D"

theme_ku <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15, color = ku_blue),
      plot.subtitle = element_text(color = "gray40", size = 11),
      axis.title    = element_text(color = "gray30"),
      panel.grid.minor = element_blank()
    )
}

# ── Plot 1: 3P% vs Adj-O scatter with OLS line, KU labeled ───────────────────
p1 <- ggplot(top50, aes(x = off_three_pct, y = adj_o)) +
  geom_smooth(method = "lm", color = "gray60", fill = "gray85",
              se = TRUE, linewidth = 0.8) +
  geom_point(data = filter(top50, !is_ku),
             color = "steelblue", size = 3, alpha = 0.75) +
  geom_point(data = filter(top50, is_ku),
             color = ku_red, size = 5, shape = 18) +
  geom_text_repel(
    data          = filter(top50, is_ku),
    aes(label     = team),
    color         = ku_red,
    fontface      = "bold",
    size          = 4,
    nudge_y       = 0.5,
    segment.color = ku_red
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "Team 3-Point Field Goal %") +
  scale_y_continuous(name   = "T-Rank Adjusted Offensive Efficiency") +
  labs(
    title    = "3P% vs Offensive Efficiency: Top 50 T-Rank Offenses (2025-26)",
    subtitle = sprintf("Pearson r = %.2f  |  Each point = one team  |  KU in red",
                       cor_test$estimate)
  ) +
  theme_ku()

ggsave("ku_3pct_vs_adj_o.png", p1, width = 9, height = 6, dpi = 150)
cat("\nSaved: ku_3pct_vs_adj_o.png\n")

# ── Plot 2: Dot plot — all 50 teams ranked by 3P%, KU highlighted ─────────────
dot_data <- top50 %>%
  arrange(off_three_pct) %>%
  mutate(team = factor(team, levels = team))

p2 <- ggplot(dot_data, aes(x = off_three_pct, y = team, color = is_ku)) +
  geom_segment(aes(x    = mean(top50$off_three_pct),
                   xend = off_three_pct,
                   y    = team,
                   yend = team),
               color = "gray80", linewidth = 0.5) +
  geom_point(size = 3) +
  geom_vline(xintercept = mean(top50$off_three_pct),
             linetype = "dashed", color = "gray50", linewidth = 0.7) +
  annotate("text",
           x     = mean(top50$off_three_pct) + 0.001,
           y     = 1,
           label = "Avg",
           hjust = 0, color = "gray50", size = 3.2) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = ku_red),
                     guide  = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = "3-Point % Among Top 50 T-Rank Offenses (2025-26)",
    subtitle = "Kansas highlighted in red | Dashed line = group average",
    x        = "Team 3-Point Field Goal %",
    y        = NULL
  ) +
  theme_ku() +
  theme(axis.text.y = element_text(
    size  = 8,
    color = ifelse(levels(dot_data$team) == "Kansas", ku_red, "gray30")
  ))

ggsave("ku_3pct_ranking.png", p2, width = 8, height = 12, dpi = 150)
cat("Saved: ku_3pct_ranking.png\n")

# ── Plot 3: Shot selection (3PAr) vs 3P% — do high-volume 3P teams shoot better? ─
p3 <- ggplot(top50, aes(x = off_three_rate, y = off_three_pct)) +
  geom_smooth(method = "lm", color = "gray60", fill = "gray85",
              se = TRUE, linewidth = 0.8) +
  geom_point(data = filter(top50, !is_ku),
             color = "steelblue", size = 3, alpha = 0.75) +
  geom_point(data = filter(top50, is_ku),
             color = ku_red, size = 5, shape = 18) +
  geom_text_repel(
    data          = filter(top50, is_ku),
    aes(label     = team),
    color         = ku_red,
    fontface      = "bold",
    size          = 4,
    nudge_y       = 0.005,
    segment.color = ku_red
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "3-Point Attempt Rate (3PA / FGA)") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     name   = "3-Point Field Goal %") +
  labs(
    title    = "Shot Selection vs 3P% Efficiency: Top 50 T-Rank Offenses (2025-26)",
    subtitle = "Do teams that shoot more 3s also make them at a higher rate?"
  ) +
  theme_ku()

ggsave("ku_3par_vs_3pct.png", p3, width = 9, height = 6, dpi = 150)
cat("Saved: ku_3par_vs_3pct.png\n")
