###############################################################################
# parity_models.R
# Econometric models for testing whether the transfer portal / NIL
# increased parity in college basketball
#
# MODEL 1: Aggregate parity DiD
#   Parity_t = alpha + beta * Post_t + gamma * trend_t + epsilon_t
#   (time-series: does parity increase after portal/NIL?)
#
# MODEL 2: Team-level DiD with heterogeneous treatment
#   wins_it = alpha_i + delta_t + beta_1 * (Post_t x BlueBood_i)
#                                + beta_2 * (Post_t x HighMajor_i)
#                                + beta_3 * (Post_t x MidMajor_i) + epsilon_it
#   (blue bloods lose relative advantage => beta_1 < 0 or beta_2 > 0)
#
# MODEL 3: Transfer talent redistribution
#   net_talent_it = alpha_i + delta_t + gamma * (Post_t x Tier_i) + epsilon_it
#   (do non-blue-bloods gain more transfer talent post-portal?)
#
# MODEL 4: Bartik IV for causal effect of transfers on wins
#   (reuses the shift-share framework from identification.R)
#
# MODEL 5: Distributional analysis
#   Track Gini, HHI, Noll-Scully over time; test for structural break
###############################################################################

source("R/program_classification.R")

# ── Policy period definitions ────────────────────────────────────────────────
# Pre-portal:  2015-2019 (transfer portal announced Oct 2018, but minimal
#              impact until one-time transfer rule)
# Transition:  2020 (COVID disruption — often excluded or controlled for)
# Post-portal: 2021-2024 (one-time transfer + NIL both effective)

define_policy_periods <- function(panel, season_col = "season") {
  panel$pre_portal  <- as.integer(panel[[season_col]] <= 2019)
  panel$post_portal <- as.integer(panel[[season_col]] >= 2021)
  panel$transition  <- as.integer(panel[[season_col]] == 2020)
  panel$covid_year  <- as.integer(panel[[season_col]] == 2020)

  # Continuous treatment intensity: years since policy change
  panel$years_post <- pmax(0, panel[[season_col]] - 2020)

  panel
}

# ══════════════════════════════════════════════════════════════════════════════
# MODEL 1: Aggregate parity time series
# ══════════════════════════════════════════════════════════════════════════════

model_aggregate_parity <- function(parity_panel) {
  # Exclude COVID year
  d <- parity_panel[parity_panel$season != 2020, ]
  d$post <- as.integer(d$season >= 2021)
  d$trend <- d$season - min(d$season)

  cat("=== MODEL 1: Aggregate Parity (Diff-in-Diff around Portal/NIL) ===\n\n")

  # Gini coefficient
  mod_gini <- lm(gini ~ post + trend, data = d)
  cat("Gini coefficient of win percentages:\n")
  print(summary(mod_gini)$coefficients[1:3, ])
  cat(sprintf("\n  Pre-portal mean Gini:  %.4f\n",
              mean(d$gini[d$post == 0], na.rm = TRUE)))
  cat(sprintf("  Post-portal mean Gini: %.4f\n",
              mean(d$gini[d$post == 1], na.rm = TRUE)))

  # Noll-Scully
  mod_ns <- lm(noll_scully ~ post + trend, data = d)
  cat("\nNoll-Scully ratio:\n")
  print(summary(mod_ns)$coefficients[1:3, ])

  # SD of win percentage
  mod_sd <- lm(sd_win_pct ~ post + trend, data = d)
  cat("\nSD of win percentage:\n")
  print(summary(mod_sd)$coefficients[1:3, ])

  # Top-25 concentration
  mod_top25 <- lm(top25_share ~ post + trend, data = d)
  cat("\nTop-25 win share concentration:\n")
  print(summary(mod_top25)$coefficients[1:3, ])

  list(gini = mod_gini, noll_scully = mod_ns,
       sd_winpct = mod_sd, top25 = mod_top25)
}

# ══════════════════════════════════════════════════════════════════════════════
# MODEL 2: Team-level DiD with heterogeneous treatment by program tier
# ══════════════════════════════════════════════════════════════════════════════

model_team_did <- function(panel) {
  d <- panel[panel$season != 2020, ]  # Exclude COVID
  d <- define_policy_periods(d)

  cat("\n=== MODEL 2: Team-Level DiD with Program Tier Interactions ===\n\n")

  # Interaction terms
  d$post_bb  <- d$post_portal * as.integer(d$program_tier == "Blue Blood")
  d$post_nbb <- d$post_portal * as.integer(d$program_tier == "Near-Blue Blood")
  d$post_hm  <- d$post_portal * as.integer(d$program_tier == "High-Major")
  d$post_mm  <- d$post_portal * as.integer(d$program_tier == "Mid-Major")

  # Win percentage model with team + season FE
  # Reference group: Blue Bloods pre-portal
  # beta_post_bb: change in BB wins post-portal
  # beta_post_hm: change in HM wins post-portal
  # If parity increases: beta_post_bb < 0 (BBs decline)
  #                  or: beta_post_hm > beta_post_bb (HMs gain more)
  mod_wins <- lm(wins ~ post_bb + post_nbb + post_hm + post_mm +
                   factor(team) + factor(season),
                 data = d)

  # Extract key coefficients
  coefs <- summary(mod_wins)$coefficients
  tier_vars <- c("post_bb", "post_nbb", "post_hm", "post_mm")
  tier_labels <- c("Blue Blood x Post", "Near-BB x Post",
                    "High-Major x Post", "Mid-Major x Post")

  cat("Effect of portal/NIL era on wins by program tier:\n")
  cat("(Positive = more wins post-portal; negative = fewer)\n\n")
  for (i in seq_along(tier_vars)) {
    if (tier_vars[i] %in% rownames(coefs)) {
      cf <- coefs[tier_vars[i], ]
      cat(sprintf("  %-25s  %+.3f (SE: %.3f, t: %.2f, p: %.4f)\n",
                  tier_labels[i], cf[1], cf[2], cf[3], cf[4]))
    }
  }

  # Test: post_bb - post_hm = 0 (do blue bloods lose relative to high-majors?)
  if (all(tier_vars[c(1,3)] %in% names(coef(mod_wins)))) {
    bb_coef <- coef(mod_wins)["post_bb"]
    hm_coef <- coef(mod_wins)["post_hm"]
    vcov_mat <- vcov(mod_wins)
    se_diff <- sqrt(vcov_mat["post_bb", "post_bb"] +
                    vcov_mat["post_hm", "post_hm"] -
                    2 * vcov_mat["post_bb", "post_hm"])
    diff_val <- bb_coef - hm_coef
    t_diff <- diff_val / se_diff

    cat(sprintf("\n  BB - HM gap change:  %+.3f (SE: %.3f, t: %.2f)\n",
                diff_val, se_diff, t_diff))
    cat("  (Negative => blue bloods lost ground relative to high-majors)\n")
  }

  # Efficiency model
  if ("adj_efficiency" %in% names(d)) {
    mod_eff <- lm(adj_efficiency ~ post_bb + post_nbb + post_hm + post_mm +
                    factor(team) + factor(season),
                  data = d)
  } else {
    mod_eff <- NULL
  }

  list(wins = mod_wins, efficiency = mod_eff)
}

# ══════════════════════════════════════════════════════════════════════════════
# MODEL 3: Transfer talent redistribution across tiers
# ══════════════════════════════════════════════════════════════════════════════

model_talent_redistribution <- function(panel) {
  d <- panel[panel$season != 2020, ]
  d <- define_policy_periods(d)

  cat("\n=== MODEL 3: Transfer Talent Redistribution by Tier ===\n\n")

  if (!"net_talent" %in% names(d)) {
    cat("  (Requires transfer-level talent data — skipping)\n")
    return(NULL)
  }

  d$post_bb  <- d$post_portal * as.integer(d$program_tier == "Blue Blood")
  d$post_nbb <- d$post_portal * as.integer(d$program_tier == "Near-Blue Blood")
  d$post_hm  <- d$post_portal * as.integer(d$program_tier == "High-Major")
  d$post_mm  <- d$post_portal * as.integer(d$program_tier == "Mid-Major")

  mod <- lm(net_talent ~ post_bb + post_nbb + post_hm + post_mm +
              factor(team) + factor(season), data = d)

  coefs <- summary(mod)$coefficients
  tier_vars <- c("post_bb", "post_nbb", "post_hm", "post_mm")
  tier_labels <- c("Blue Blood x Post", "Near-BB x Post",
                    "High-Major x Post", "Mid-Major x Post")

  cat("Change in net transfer talent post-portal by tier:\n\n")
  for (i in seq_along(tier_vars)) {
    if (tier_vars[i] %in% rownames(coefs)) {
      cf <- coefs[tier_vars[i], ]
      cat(sprintf("  %-25s  %+.3f (SE: %.3f, t: %.2f)\n",
                  tier_labels[i], cf[1], cf[2], cf[3]))
    }
  }

  mod
}

# ══════════════════════════════════════════════════════════════════════════════
# MODEL 5: Structural break test in parity measures
# ══════════════════════════════════════════════════════════════════════════════

model_structural_break <- function(parity_panel, break_year = 2021) {
  d <- parity_panel[parity_panel$season != 2020, ]

  cat("\n=== MODEL 5: Structural Break in Parity Metrics ===\n\n")

  metrics <- c("gini", "noll_scully", "sd_win_pct", "top25_share")
  metric_labels <- c("Gini Coefficient", "Noll-Scully Ratio",
                      "SD Win Pct", "Top-25 Concentration")

  results <- list()
  for (i in seq_along(metrics)) {
    m <- metrics[i]
    if (!m %in% names(d)) next

    # Chow-type test: full model vs restricted (no break)
    d$post <- as.integer(d$season >= break_year)
    d$trend <- d$season - min(d$season)

    restricted  <- lm(as.formula(paste(m, "~ trend")), data = d)
    unrestricted <- lm(as.formula(paste(m, "~ trend + post + trend:post")),
                       data = d)

    f_test <- anova(restricted, unrestricted)

    cat(sprintf("  %-25s  ", metric_labels[i]))
    if (!is.na(f_test$`Pr(>F)`[2])) {
      cat(sprintf("F = %.2f, p = %.4f",
                  f_test$F[2], f_test$`Pr(>F)`[2]))
      if (f_test$`Pr(>F)`[2] < 0.05) cat(" *")
      if (f_test$`Pr(>F)`[2] < 0.01) cat("*")
    } else {
      cat("(insufficient data)")
    }
    cat("\n")

    results[[m]] <- list(restricted = restricted,
                          unrestricted = unrestricted,
                          f_test = f_test)
  }

  results
}

# ══════════════════════════════════════════════════════════════════════════════
# Master analysis runner
# ══════════════════════════════════════════════════════════════════════════════

run_parity_analysis <- function(panel, parity_panel) {
  cat("================================================================\n")
  cat("  TRANSFER PORTAL & COMPETITIVE BALANCE IN COLLEGE BASKETBALL\n")
  cat("================================================================\n\n")

  results <- list()

  results$aggregate  <- model_aggregate_parity(parity_panel)
  results$team_did   <- model_team_did(panel)
  results$talent     <- model_talent_redistribution(panel)
  results$break_test <- model_structural_break(parity_panel)

  results
}
