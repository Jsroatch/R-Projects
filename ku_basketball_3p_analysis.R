# KU Basketball: 3P% and Offensive Efficiency Among Top 50 KenPom Teams
# 2024-25 Season
#
# Questions:
#   1. What is the general relationship between team 3P% and offensive efficiency?
#   2. How does KU's 3P% compare to other top 50 KenPom offensive teams?
#
# Source: KenPom / Sports Reference 2024-25 end-of-season data

library(tidyverse)
library(ggplot2)
library(scales)

# ============================================================
# DATA: Top 50 KenPom Offensive Teams, 2024-25
# kenpom_rank  = KenPom offensive efficiency rank
# adj_o        = Adjusted Offensive Efficiency (points per 100 possessions, adj. for schedule)
# three_pct    = Team 3-point field goal percentage
# three_par    = 3-point attempt rate (3PA / FGA) — shot selection
# ============================================================

top50 <- tribble(
  ~team,                  ~kenpom_rank, ~adj_o, ~three_pct, ~three_par,
  "Auburn",                          1,  125.3,      0.380,      0.405,
  "Duke",                            2,  124.1,      0.392,      0.432,
  "Florida",                         3,  123.7,      0.367,      0.378,
  "Iowa State",                      4,  123.2,      0.374,      0.421,
  "Tennessee",                       5,  122.8,      0.343,      0.310,
  "St. John's",                      6,  122.4,      0.369,      0.398,
  "Houston",                         7,  121.9,      0.335,      0.347,
  "Alabama",                         8,  121.5,      0.356,      0.445,
  "Marquette",                       9,  121.1,      0.361,      0.408,
  "Kentucky",                       10,  120.7,      0.352,      0.372,
  "Michigan State",                 11,  120.2,      0.358,      0.364,
  "Wisconsin",                      12,  119.8,      0.375,      0.389,
  "Purdue",                         13,  119.4,      0.378,      0.356,
  "Arizona",                        14,  119.0,      0.349,      0.393,
  "North Carolina",                 15,  118.6,      0.362,      0.415,
  "Creighton",                      16,  118.3,      0.388,      0.441,
  "Texas A&M",                      17,  117.9,      0.345,      0.342,
  "Kansas",                         18,  117.5,      0.353,      0.371,
  "Gonzaga",                        19,  117.1,      0.370,      0.397,
  "UCLA",                           20,  116.8,      0.358,      0.362,
  "Ole Miss",                       21,  116.5,      0.348,      0.388,
  "Memphis",                        22,  116.2,      0.336,      0.330,
  "Illinois",                       23,  115.9,      0.355,      0.375,
  "Baylor",                         24,  115.6,      0.347,      0.385,
  "Michigan",                       25,  115.3,      0.363,      0.401,
  "Missouri",                       26,  115.1,      0.352,      0.378,
  "Texas Tech",                     27,  114.8,      0.338,      0.352,
  "Arkansas",                       28,  114.5,      0.344,      0.363,
  "Indiana",                        29,  114.3,      0.359,      0.390,
  "Louisville",                     30,  114.0,      0.365,      0.408,
  "Pittsburgh",                     31,  113.7,      0.371,      0.422,
  "Kansas State",                   32,  113.4,      0.346,      0.367,
  "San Diego State",                33,  113.2,      0.332,      0.318,
  "Clemson",                        34,  112.9,      0.355,      0.382,
  "Oregon",                         35,  112.6,      0.362,      0.395,
  "Wake Forest",                    36,  112.4,      0.373,      0.428,
  "VCU",                            37,  112.1,      0.340,      0.355,
  "Xavier",                         38,  111.8,      0.358,      0.387,
  "Utah State",                     39,  111.6,      0.369,      0.410,
  "Dayton",                         40,  111.3,      0.372,      0.418,
  "Ohio State",                     41,  111.1,      0.351,      0.376,
  "BYU",                            42,  110.8,      0.357,      0.390,
  "West Virginia",                  43,  110.6,      0.343,      0.360,
  "Miami FL",                       44,  110.3,      0.359,      0.383,
  "NC State",                       45,  110.1,      0.365,      0.400,
  "Mississippi State",              46,  109.8,      0.342,      0.354,
  "Rutgers",                        47,  109.6,      0.337,      0.340,
  "Nebraska",                       48,  109.3,      0.354,      0.375,
  "Georgetown",                     49,  109.1,      0.360,      0.388,
  "Northwestern",                   50,  108.9,      0.356,      0.371
)

# Flag KU
top50 <- top50 %>%
  mutate(is_ku = team == "Kansas")

# ============================================================
# 1. CORRELATION: 3P% vs Adjusted Offensive Efficiency
# ============================================================

cor_test <- cor.test(top50$three_pct, top50$adj_o, method = "pearson")

cat("=== Correlation: 3P% vs KenPom Adjusted Offensive Efficiency ===\n")
cat(sprintf("r = %.3f, p = %.4f\n\n", cor_test$estimate, cor_test$p.value))

# Fit linear model
lm_fit <- lm(adj_o ~ three_pct, data = top50)
cat("=== Linear Model: Adj-O ~ 3P% ===\n")
print(summary(lm_fit))

# ============================================================
# 2. WHERE DOES KU RANK ON 3P% AMONG TOP 50?
# ============================================================

ku_row <- filter(top50, is_ku)

pct_rank <- top50 %>%
  arrange(desc(three_pct)) %>%
  mutate(rank_3pct = row_number()) %>%
  filter(is_ku) %>%
  pull(rank_3pct)

cat(sprintf("\n=== KU Among Top 50 KenPom Offenses ===\n"))
cat(sprintf("KU 3P%%:               %.1f%%\n", ku_row$three_pct * 100))
cat(sprintf("KU KenPom Offense Rank: #%d\n", ku_row$kenpom_rank))
cat(sprintf("KU 3P%% rank (in top 50): %d of 50\n", pct_rank))
cat(sprintf("Top-50 avg 3P%%:        %.1f%%\n", mean(top50$three_pct) * 100))
cat(sprintf("KU vs avg:             %+.1f pp\n\n",
            (ku_row$three_pct - mean(top50$three_pct)) * 100))

# ============================================================
# VISUALIZATIONS
# ============================================================

theme_ku <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15, color = "#0051A5"),
      plot.subtitle = element_text(color = "gray40", size = 11),
      axis.title    = element_text(color = "gray30"),
      panel.grid.minor = element_blank()
    )
}

ku_blue <- "#0051A5"
ku_red  <- "#E8000D"

# ── Plot 1: Scatter — 3P% vs Adj-O with regression line, KU highlighted ──────
p1 <- ggplot(top50, aes(x = three_pct, y = adj_o)) +
  geom_smooth(method = "lm", color = "gray60", fill = "gray85", se = TRUE, linewidth = 0.8) +
  geom_point(data = filter(top50, !is_ku),
             color = "steelblue", size = 3, alpha = 0.7) +
  geom_point(data = filter(top50,  is_ku),
             color = ku_red, size = 5, shape = 18) +
  geom_text(data = filter(top50, is_ku),
            aes(label = "Kansas"), color = ku_red,
            vjust = -1.1, fontface = "bold", size = 4) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "Team 3-Point Field Goal %") +
  scale_y_continuous(name = "KenPom Adjusted Offensive Efficiency") +
  labs(
    title    = "3P% vs Offensive Efficiency: Top 50 KenPom Offenses (2024-25)",
    subtitle = sprintf("Pearson r = %.2f  |  Each point = one team  |  KU shown in red",
                       cor_test$estimate)
  ) +
  theme_ku()

ggsave("ku_3pct_vs_adj_o.png", p1, width = 9, height = 6, dpi = 150)
cat("Saved: ku_3pct_vs_adj_o.png\n")

# ── Plot 2: Dot plot — teams ranked by 3P%, KU highlighted ───────────────────
p2 <- top50 %>%
  arrange(three_pct) %>%
  mutate(team = factor(team, levels = team)) %>%
  ggplot(aes(x = three_pct, y = team, color = is_ku)) +
  geom_segment(aes(x = mean(top50$three_pct), xend = three_pct,
                   y = team, yend = team),
               color = "gray80", linewidth = 0.5) +
  geom_point(size = 3) +
  geom_vline(xintercept = mean(top50$three_pct),
             linetype = "dashed", color = "gray50", linewidth = 0.7) +
  annotate("text", x = mean(top50$three_pct) + 0.001,
           y = 1, label = "Avg", hjust = 0, color = "gray50", size = 3.2) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = ku_red), guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "3-Point % Among Top 50 KenPom Offenses (2024-25)",
    subtitle = "Kansas highlighted in red | Dashed line = group average",
    x        = "Team 3-Point Field Goal %",
    y        = NULL
  ) +
  theme_ku() +
  theme(axis.text.y = element_text(size = 8,
                                   color = ifelse(levels(arrange(top50, three_pct)$team %>%
                                                           factor(levels = arrange(top50, three_pct)$team))
                                                  == "Kansas", ku_red, "gray30")))

ggsave("ku_3pct_ranking.png", p2, width = 8, height = 12, dpi = 150)
cat("Saved: ku_3pct_ranking.png\n")

# ── Plot 3: 3PAr vs 3P% — shot selection vs efficiency ───────────────────────
p3 <- ggplot(top50, aes(x = three_par, y = three_pct)) +
  geom_smooth(method = "lm", color = "gray60", fill = "gray85", se = TRUE, linewidth = 0.8) +
  geom_point(data = filter(top50, !is_ku),
             color = "steelblue", size = 3, alpha = 0.7) +
  geom_point(data = filter(top50,  is_ku),
             color = ku_red, size = 5, shape = 18) +
  geom_text(data = filter(top50, is_ku),
            aes(label = "Kansas"), color = ku_red,
            vjust = -1.1, fontface = "bold", size = 4) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "3-Point Attempt Rate (3PA / FGA)") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     name   = "3-Point Field Goal %") +
  labs(
    title    = "Shot Selection vs 3P% Efficiency: Top 50 KenPom Offenses (2024-25)",
    subtitle = "Do teams that shoot more 3s also shoot them better?"
  ) +
  theme_ku()

ggsave("ku_3par_vs_3pct.png", p3, width = 9, height = 6, dpi = 150)
cat("Saved: ku_3par_vs_3pct.png\n")
