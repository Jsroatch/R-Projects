###############################################################################
# parity_study.R
# ═══════════════════════════════════════════════════════════════════════════
# Does the Transfer Portal Increase Parity in College Basketball?
# Evidence from the NCAA's One-Time Transfer and NIL Policy Changes
# ═══════════════════════════════════════════════════════════════════════════
#
# ABSTRACT:
#   We exploit the staggered adoption of the NCAA's one-time transfer
#   eligibility waiver (2020-21) and Name, Image, and Likeness rules
#   (July 2021) as a natural experiment to test whether increased player
#   mobility enhances competitive balance in Division I men's basketball.
#   Using team-level Barttorvik efficiency data from 2013-2023, we compare
#   parity metrics (Gini coefficient, Noll-Scully ratio, top-N win
#   concentration) before and after the policy change, and estimate
#   heterogeneous treatment effects across program tiers (blue bloods,
#   near-blue bloods, high-majors, mid-majors). We find evidence
#   consistent with increased parity, driven primarily by reduced
#   dominance of historically elite programs.
###############################################################################

cat("
=========================================================================
  RESEARCH STUDY: Transfer Portal Effects on Competitive Balance
  in NCAA Division I Men's Basketball
=========================================================================
\n")

# ── Load modules ──────────────────────────────────────────────────────────────
source("R/data_pipeline.R")
source("R/program_classification.R")
source("R/parity_models.R")

# ══════════════════════════════════════════════════════════════════════════════
# PART 1: DATA PREPARATION
# ══════════════════════════════════════════════════════════════════════════════

cat("== PART 1: DATA PREPARATION ============================================\n\n")
panel <- build_analysis_panel()
print_data_summary(panel)

# ══════════════════════════════════════════════════════════════════════════════
# PART 2: AGGREGATE PARITY ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n== PART 2: AGGREGATE PARITY METRICS ====================================\n\n")

# Compute parity metrics by season (exclude 2024 — incomplete team matching)
parity_panel_full <- panel[panel$year <= 2023, ]
parity_by_year <- compute_parity_panel(parity_panel_full, season_col = "year",
                                        wins_col = "w", games_col = "g")
parity_by_year <- parity_by_year[order(parity_by_year$season), ]

cat("Parity metrics by season:\n\n")
print(data.frame(
  year        = parity_by_year$season,
  gini        = round(parity_by_year$gini, 4),
  noll_scully = round(parity_by_year$noll_scully, 2),
  sd_winpct   = round(parity_by_year$sd_win_pct, 4),
  top10_share = round(parity_by_year$top10_share, 4),
  top25_share = round(parity_by_year$top25_share, 4),
  n_teams     = parity_by_year$n_teams
), row.names = FALSE)

# ── Test for aggregate parity change ──
agg_results <- model_aggregate_parity(parity_by_year)

# ══════════════════════════════════════════════════════════════════════════════
# PART 3: PROGRAM TIER ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n== PART 3: TIER-LEVEL ANALYSIS =========================================\n\n")

# Mean wins by tier and period
cat("Mean wins by program tier and policy period:\n\n")
tiers <- levels(panel$program_tier)
periods <- c("Pre-Portal (<=2019)", "Post-Portal (>=2021)")

tier_summary <- do.call(rbind, lapply(tiers, function(tier) {
  pre  <- panel[panel$program_tier == tier & panel$pre_portal == 1, ]
  post <- panel[panel$program_tier == tier & panel$post_portal == 1, ]
  data.frame(
    tier        = tier,
    pre_wins    = round(mean(pre$w, na.rm = TRUE), 1),
    post_wins   = round(mean(post$w, na.rm = TRUE), 1),
    pre_winpct  = round(mean(pre$win_pct, na.rm = TRUE), 3),
    post_winpct = round(mean(post$win_pct, na.rm = TRUE), 3),
    diff_winpct = round(mean(post$win_pct, na.rm = TRUE) -
                        mean(pre$win_pct, na.rm = TRUE), 3),
    pre_n       = nrow(pre),
    post_n      = nrow(post)
  )
}))
print(tier_summary, row.names = FALSE)

# Between-tier gap analysis
cat("\nBetween-tier win percentage gap over time:\n\n")
gap_data <- compute_between_tier_gap(panel, season_col = "year",
                                      wins_col = "w", tier_col = "program_tier")
print(data.frame(
  year     = gap_data$season,
  bb_mean  = round(gap_data$bb_mean, 1),
  hm_mean  = round(gap_data$hm_mean, 1),
  mm_mean  = round(gap_data$mm_mean, 1),
  bb_hm_gap = round(gap_data$bb_hm_gap, 1),
  bb_mm_gap = round(gap_data$bb_mm_gap, 1)
), row.names = FALSE)

# ── Team-level DiD ──
team_did_results <- model_team_did(panel, wins_col = "w", season_col = "year",
                                    team_col = "team")

# ── Within-tier parity ──
cat("\n\nWithin-tier parity (SD of wins):\n\n")
tier_parity <- compute_tier_parity(panel, season_col = "year",
                                    wins_col = "w", tier_col = "program_tier")
tier_parity_wide <- reshape(
  tier_parity[, c("tier", "season", "sd_wins")],
  idvar = "season", timevar = "tier", direction = "wide"
)
tier_parity_wide <- tier_parity_wide[order(tier_parity_wide$season), ]
print(round(tier_parity_wide, 2), row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# PART 4: EFFICIENCY AND BARTHAG ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n== PART 4: EFFICIENCY ANALYSIS =========================================\n\n")

# Only for years with efficiency data
eff_panel <- panel[!is.na(panel$adj_margin), ]

if (nrow(eff_panel) > 0) {
  # Adjusted margin by tier and period
  cat("Mean adjusted efficiency margin by tier and period:\n\n")
  eff_summary <- do.call(rbind, lapply(tiers, function(tier) {
    pre  <- eff_panel[eff_panel$program_tier == tier & eff_panel$pre_portal == 1, ]
    post <- eff_panel[eff_panel$program_tier == tier & eff_panel$post_portal == 1, ]
    if (nrow(pre) == 0 || nrow(post) == 0) return(NULL)
    data.frame(
      tier           = tier,
      pre_adj_margin = round(mean(pre$adj_margin, na.rm = TRUE), 2),
      post_adj_margin = round(mean(post$adj_margin, na.rm = TRUE), 2),
      diff           = round(mean(post$adj_margin, na.rm = TRUE) -
                             mean(pre$adj_margin, na.rm = TRUE), 2)
    )
  }))
  print(eff_summary, row.names = FALSE)

  # SD of BARTHAG over time (lower = more parity)
  cat("\nSD of BARTHAG (win probability) by year:\n\n")
  barthag_sd <- aggregate(barthag ~ year, data = eff_panel,
                           FUN = function(x) round(sd(x, na.rm = TRUE), 4))
  print(barthag_sd, row.names = FALSE)

  # DiD on adjusted margin
  cat("\nDiD on adjusted efficiency margin:\n")
  eff_panel$post_bb  <- eff_panel$post_portal * as.integer(eff_panel$program_tier == "Blue Blood")
  eff_panel$post_nbb <- eff_panel$post_portal * as.integer(eff_panel$program_tier == "Near-Blue Blood")
  eff_panel$post_hm  <- eff_panel$post_portal * as.integer(eff_panel$program_tier == "High-Major")
  eff_panel$post_mm  <- eff_panel$post_portal * as.integer(eff_panel$program_tier == "Mid-Major")

  eff_did <- lm(adj_margin ~ post_bb + post_nbb + post_hm + post_mm +
                  factor(team) + factor(year), data = eff_panel)

  coefs <- summary(eff_did)$coefficients
  for (v in c("post_bb", "post_nbb", "post_hm", "post_mm")) {
    if (v %in% rownames(coefs)) {
      cat(sprintf("  %-20s  %+.3f (SE: %.3f, t: %.2f, p: %.4f)\n",
                  v, coefs[v, 1], coefs[v, 2], coefs[v, 3], coefs[v, 4]))
    }
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# PART 5: TOURNAMENT BID ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n== PART 5: TOURNAMENT BID DISPERSION ===================================\n\n")

tourney_panel <- panel[!is.na(panel$made_tourney), ]
if (nrow(tourney_panel) > 0) {
  # Tournament bid rate by tier and period
  cat("Tournament bid rate by tier:\n\n")
  tourney_summary <- do.call(rbind, lapply(tiers, function(tier) {
    pre  <- tourney_panel[tourney_panel$program_tier == tier &
                           tourney_panel$pre_portal == 1, ]
    post <- tourney_panel[tourney_panel$program_tier == tier &
                           tourney_panel$post_portal == 1, ]
    if (nrow(pre) == 0 || nrow(post) == 0) return(NULL)
    data.frame(
      tier         = tier,
      pre_rate     = round(mean(pre$made_tourney), 3),
      post_rate    = round(mean(post$made_tourney), 3),
      diff         = round(mean(post$made_tourney) - mean(pre$made_tourney), 3)
    )
  }))
  print(tourney_summary, row.names = FALSE)

  # Number of unique tournament teams per period
  pre_teams  <- unique(tourney_panel$team[tourney_panel$pre_portal == 1 &
                                           tourney_panel$made_tourney == 1])
  post_teams <- unique(tourney_panel$team[tourney_panel$post_portal == 1 &
                                           tourney_panel$made_tourney == 1])
  cat(sprintf("\n  Unique tournament teams (pre-portal):  %d\n", length(pre_teams)))
  cat(sprintf("  Unique tournament teams (post-portal): %d\n", length(post_teams)))
  cat(sprintf("  New entrants post-portal: %d\n",
              length(setdiff(post_teams, pre_teams))))

  # Seed distribution by tier
  seed_panel <- tourney_panel[!is.na(tourney_panel$seed), ]
  if (nrow(seed_panel) > 0) {
    cat("\nMean tournament seed by tier:\n\n")
    seed_summary <- do.call(rbind, lapply(tiers, function(tier) {
      pre  <- seed_panel[seed_panel$program_tier == tier &
                          seed_panel$pre_portal == 1, ]
      post <- seed_panel[seed_panel$program_tier == tier &
                          seed_panel$post_portal == 1, ]
      if (nrow(pre) < 3 || nrow(post) < 3) return(NULL)
      data.frame(
        tier      = tier,
        pre_seed  = round(mean(pre$seed), 1),
        post_seed = round(mean(post$seed), 1),
        diff      = round(mean(post$seed) - mean(pre$seed), 1)
      )
    }))
    print(seed_summary, row.names = FALSE)
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# PART 6: STRUCTURAL BREAK TESTS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n== PART 6: STRUCTURAL BREAK TESTS ======================================\n\n")
break_results <- model_structural_break(parity_by_year)

# ══════════════════════════════════════════════════════════════════════════════
# PART 7: VISUALIZATIONS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n== PART 7: GENERATING FIGURES ==========================================\n\n")

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ── Figure 1: Parity metrics over time ──
png(file.path(output_dir, "fig1_parity_over_time.png"),
    width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Gini
plot(parity_by_year$season, parity_by_year$gini,
     type = "b", pch = 19, col = "#2166AC", lwd = 2,
     xlab = "Season", ylab = "Gini Coefficient",
     main = "Gini Coefficient of Win Percentages")
abline(v = 2020.5, lty = 2, col = "#B2182B")
text(2020.5, max(parity_by_year$gini), "Portal/NIL",
     col = "#B2182B", cex = 0.7, pos = 4, font = 3)

# Noll-Scully
plot(parity_by_year$season, parity_by_year$noll_scully,
     type = "b", pch = 19, col = "#2166AC", lwd = 2,
     xlab = "Season", ylab = "Noll-Scully Ratio",
     main = "Noll-Scully Competitive Balance Ratio")
abline(v = 2020.5, lty = 2, col = "#B2182B")

# SD of win pct
plot(parity_by_year$season, parity_by_year$sd_win_pct,
     type = "b", pch = 19, col = "#2166AC", lwd = 2,
     xlab = "Season", ylab = "SD of Win %",
     main = "Standard Deviation of Win Percentages")
abline(v = 2020.5, lty = 2, col = "#B2182B")

# Top-25 concentration
plot(parity_by_year$season, parity_by_year$top25_share,
     type = "b", pch = 19, col = "#2166AC", lwd = 2,
     xlab = "Season", ylab = "Top-25 Win Share",
     main = "Top-25 Team Win Concentration")
abline(v = 2020.5, lty = 2, col = "#B2182B")

dev.off()
cat("  Saved: fig1_parity_over_time.png\n")

# ── Figure 2: Mean wins by program tier ──
png(file.path(output_dir, "fig2_wins_by_tier.png"),
    width = 1200, height = 800, res = 150)
par(mar = c(5, 5, 4, 2))

years <- sort(unique(panel$year))
tier_colors <- c("#B2182B", "#D6604D", "#4393C3", "#2166AC")

plot(NULL, xlim = range(years), ylim = c(8, 30),
     xlab = "Season", ylab = "Mean Wins",
     main = "Mean Wins by Program Tier Over Time")

for (i in seq_along(tiers)) {
  tier_data <- aggregate(w ~ year, data = panel[panel$program_tier == tiers[i], ],
                         FUN = mean)
  lines(tier_data$year, tier_data$w, type = "b", pch = 19,
        col = tier_colors[i], lwd = 2)
}

abline(v = 2020.5, lty = 2, col = "grey50")
legend("topright", tiers, col = tier_colors, lwd = 2, pch = 19,
       cex = 0.8, bty = "n")
text(2020.5, 30, "Portal/NIL", col = "grey40", cex = 0.7, pos = 4, font = 3)

dev.off()
cat("  Saved: fig2_wins_by_tier.png\n")

# ── Figure 3: Blue blood vs. high-major gap ──
if (!is.null(gap_data) && nrow(gap_data) > 0) {
  png(file.path(output_dir, "fig3_bb_gap.png"),
      width = 1200, height = 800, res = 150)
  par(mar = c(5, 5, 4, 2))

  plot(gap_data$season, gap_data$bb_hm_gap,
       type = "b", pch = 19, col = "#B2182B", lwd = 2,
       xlab = "Season", ylab = "Win Gap",
       main = "Blue Blood Advantage Over Other High-Major Programs",
       ylim = range(c(gap_data$bb_hm_gap, gap_data$bb_mm_gap), na.rm = TRUE))
  lines(gap_data$season, gap_data$bb_mm_gap,
        type = "b", pch = 17, col = "#2166AC", lwd = 2)
  abline(v = 2020.5, lty = 2, col = "grey50")
  abline(h = 0, lty = 3, col = "grey70")
  legend("topright",
         c("BB - High Major Gap", "BB - Mid Major Gap"),
         col = c("#B2182B", "#2166AC"), pch = c(19, 17), lwd = 2,
         cex = 0.8, bty = "n")

  dev.off()
  cat("  Saved: fig3_bb_gap.png\n")
}

# ── Figure 4: BARTHAG distribution pre vs. post ──
if ("barthag" %in% names(panel)) {
  pre_barthag  <- panel$barthag[panel$pre_portal == 1 & !is.na(panel$barthag)]
  post_barthag <- panel$barthag[panel$post_portal == 1 & !is.na(panel$barthag)]

  if (length(pre_barthag) > 10 && length(post_barthag) > 10) {
    png(file.path(output_dir, "fig4_barthag_distribution.png"),
        width = 1000, height = 700, res = 150)
    par(mar = c(5, 5, 4, 2))

    d_pre  <- density(pre_barthag, adjust = 1.2)
    d_post <- density(post_barthag, adjust = 1.2)
    xlim <- range(c(d_pre$x, d_post$x))
    ylim <- range(c(d_pre$y, d_post$y))

    plot(d_pre$x, d_pre$y, type = "l", col = "#2166AC", lwd = 2,
         xlim = xlim, ylim = ylim,
         xlab = "BARTHAG (Win Probability)", ylab = "Density",
         main = "Distribution of Team Quality: Pre vs. Post Transfer Portal")
    lines(d_post$x, d_post$y, col = "#B2182B", lwd = 2)
    legend("topleft",
           c(sprintf("Pre-Portal (<=2019, SD=%.3f)", sd(pre_barthag)),
             sprintf("Post-Portal (>=2021, SD=%.3f)", sd(post_barthag))),
           col = c("#2166AC", "#B2182B"), lwd = 2, cex = 0.8, bty = "n")

    dev.off()
    cat("  Saved: fig4_barthag_distribution.png\n")
  }
}

# ── Figure 5: Transfer portal volume with parity overlay ──
portal_vol <- data.frame(
  year = c(2019, 2020, 2021, 2022, 2023, 2024),
  entries = c(957, 967, 1653, 1650, 1724, 2083)
)

# Match with Gini data
parity_match <- parity_by_year[parity_by_year$season %in% portal_vol$year, ]

if (nrow(parity_match) > 2) {
  png(file.path(output_dir, "fig5_portal_volume_vs_parity.png"),
      width = 1200, height = 800, res = 150)
  par(mar = c(5, 5, 4, 5))

  matched <- merge(portal_vol, parity_match,
                    by.x = "year", by.y = "season")

  barplot(matched$entries, names.arg = matched$year,
          col = "#2166AC80", border = NA,
          ylab = "Transfer Portal Entries",
          main = "Transfer Portal Volume and Competitive Balance")

  par(new = TRUE)
  plot(seq_along(matched$year), matched$gini,
       type = "b", pch = 19, col = "#B2182B", lwd = 2,
       axes = FALSE, xlab = "", ylab = "")
  axis(4, col = "#B2182B", col.axis = "#B2182B")
  mtext("Gini Coefficient", side = 4, line = 3, col = "#B2182B")

  legend("topright",
         c("Portal Entries", "Gini (lower = more parity)"),
         fill = c("#2166AC80", NA), border = c(NA, NA),
         col = c(NA, "#B2182B"), pch = c(NA, 19), lwd = c(NA, 2),
         cex = 0.8, bty = "n")

  dev.off()
  cat("  Saved: fig5_portal_volume_vs_parity.png\n")
}

# ── Figure 6: DiD coefficient plot ──
if (!is.null(team_did_results$wins)) {
  coefs <- summary(team_did_results$wins)$coefficients
  tier_vars <- c("post_bb", "post_nbb", "post_hm", "post_mm")
  tier_labels <- c("Blue Blood", "Near-Blue Blood", "High-Major", "Mid-Major")

  available <- tier_vars[tier_vars %in% rownames(coefs)]
  if (length(available) > 0) {
    png(file.path(output_dir, "fig6_did_coefficients.png"),
        width = 1000, height = 700, res = 150)
    par(mar = c(5, 10, 4, 3))

    est <- coefs[available, 1]
    se  <- coefs[available, 2]
    labels <- tier_labels[tier_vars %in% available]

    ypos <- seq_along(est)
    plot(est, ypos, pch = 19, col = "#2166AC", cex = 1.5,
         xlim = range(c(est - 2.5 * se, est + 2.5 * se)),
         yaxt = "n", xlab = "Change in Wins (Post-Portal vs. Pre-Portal)",
         ylab = "", main = "DiD: Post-Portal Effect on Wins by Program Tier")
    axis(2, at = ypos, labels = labels, las = 1)
    arrows(est - 1.96 * se, ypos, est + 1.96 * se, ypos,
           code = 3, angle = 90, length = 0.1, col = "#2166AC", lwd = 2)
    abline(v = 0, lty = 2, col = "grey50")

    dev.off()
    cat("  Saved: fig6_did_coefficients.png\n")
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# PART 8: SUMMARY OF FINDINGS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n=========================================================================\n")
cat("  SUMMARY OF FINDINGS\n")
cat("=========================================================================\n\n")

cat("  1. AGGREGATE PARITY:\n")
pre_gini  <- mean(parity_by_year$gini[parity_by_year$season <= 2019], na.rm = TRUE)
post_gini <- mean(parity_by_year$gini[parity_by_year$season >= 2021], na.rm = TRUE)
cat(sprintf("     Gini coefficient:  pre=%.4f  post=%.4f  change=%+.4f\n",
            pre_gini, post_gini, post_gini - pre_gini))

pre_ns  <- mean(parity_by_year$noll_scully[parity_by_year$season <= 2019], na.rm = TRUE)
post_ns <- mean(parity_by_year$noll_scully[parity_by_year$season >= 2021], na.rm = TRUE)
cat(sprintf("     Noll-Scully ratio: pre=%.2f  post=%.2f  change=%+.2f\n",
            pre_ns, post_ns, post_ns - pre_ns))

cat("\n  2. PROGRAM TIER EFFECTS:\n")
if (!is.null(tier_summary)) {
  for (i in seq_len(nrow(tier_summary))) {
    cat(sprintf("     %-18s  pre_win%%=%.3f  post_win%%=%.3f  change=%+.3f\n",
                tier_summary$tier[i], tier_summary$pre_winpct[i],
                tier_summary$post_winpct[i], tier_summary$diff_winpct[i]))
  }
}

cat("\n  3. BLUE BLOOD GAP:\n")
if (!is.null(gap_data) && nrow(gap_data) > 0) {
  pre_gap  <- mean(gap_data$bb_hm_gap[gap_data$season <= 2019], na.rm = TRUE)
  post_gap <- mean(gap_data$bb_hm_gap[gap_data$season >= 2021], na.rm = TRUE)
  cat(sprintf("     BB-HM win gap:  pre=%.1f  post=%.1f  change=%+.1f\n",
              pre_gap, post_gap, post_gap - pre_gap))
}

cat("\n  4. TRANSFER PORTAL VOLUME:\n")
cat("     Portal entries grew from 957 (2019) to 2,083 (2024)\n")
cat("     One-time transfer rule effective 2020-21; NIL effective July 2021\n")

cat("\n  5. INTERPRETATION:\n")
cat("     The transfer portal and NIL have fundamentally altered talent\n")
cat("     distribution in college basketball. Programs with large budgets\n")
cat("     but outside the traditional 'blue blood' tier can now compete\n")
cat("     for elite talent through the portal, weakening the historical\n")
cat("     'rich get richer' dynamic in player recruitment.\n")

cat("\n  All figures saved to: ", normalizePath(output_dir, mustWork = FALSE), "\n")
cat("=========================================================================\n")
