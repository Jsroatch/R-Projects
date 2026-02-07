###############################################################################
# identification.R
# Econometric identification strategies for estimating the causal effect
# of transfer portal talent on team performance
# (Base R only — no external dependencies)
#
# Strategies implemented:
#   1. Naive OLS (biased benchmark)
#   2. Team + season fixed effects OLS (within estimator)
#   3. Shift-share (Bartik) 2SLS IV using coaching-change supply shocks
#   4. Difference-in-differences around coaching-change driven transfers
#   5. First-stage diagnostics and instrument validity checks
###############################################################################

# ── Helper: demean within groups (for fixed effects) ──────────────────────────
demean <- function(x, ...) {
  groups <- interaction(...)
  group_means <- tapply(x, groups, mean, na.rm = TRUE)
  x - group_means[as.character(groups)]
}

# ── Helper: robust (HC1) standard errors ──────────────────────────────────────
robust_se <- function(model) {
  X <- model.matrix(model)
  n <- nrow(X)
  k <- ncol(X)
  e <- as.numeric(residuals(model))
  # HC1: (n/(n-k)) * (X'X)^{-1} X' diag(e^2) X (X'X)^{-1}
  bread <- solve(crossprod(X))
  # Multiply each row of X by the corresponding residual
  Xe <- X * e  # n×k matrix, each column multiplied by e element-wise
  meat <- crossprod(Xe)
  V    <- (n / (n - k)) * bread %*% meat %*% bread
  se   <- sqrt(diag(V))
  names(se) <- colnames(X)
  se
}

# ── Helper: pretty coefficient table ──────────────────────────────────────────
print_coef_table <- function(model, robust = TRUE, label = "") {
  cf  <- coef(model)
  if (robust) {
    se  <- robust_se(model)
  } else {
    se  <- summary(model)$coefficients[, 2]
  }
  tstat <- cf / se
  pval  <- 2 * pt(-abs(tstat), df = model$df.residual)

  tab <- data.frame(
    Estimate  = round(cf, 4),
    Std.Error = round(se, 4),
    t.value   = round(tstat, 3),
    p.value   = round(pval, 4)
  )
  if (label != "") cat(sprintf("\n  %s\n", label))
  cat(sprintf("  N = %d, R-sq = %.4f, Adj R-sq = %.4f\n",
              length(model$residuals),
              summary(model)$r.squared,
              summary(model)$adj.r.squared))
  print(tab)
  invisible(tab)
}

# ── 1. Naive OLS ──────────────────────────────────────────────────────────────
run_naive_ols <- function(panel) {
  wins_mod <- lm(wins ~ net_talent, data = panel)
  eff_mod  <- lm(adj_efficiency ~ net_talent, data = panel)
  list(wins = wins_mod, efficiency = eff_mod)
}

# ── 2. Team + Season Fixed Effects OLS ────────────────────────────────────────
# Within-estimator: demean all variables by team and season
run_fe_ols <- function(panel) {
  p <- panel
  # Demean by team and season (two-way FE via Frisch-Waugh)
  p$wins_dm        <- demean(p$wins, p$team_id) |> demean(p$season)
  p$eff_dm         <- demean(p$adj_efficiency, p$team_id) |> demean(p$season)
  p$net_talent_dm  <- demean(p$net_talent, p$team_id) |> demean(p$season)

  wins_mod <- lm(wins_dm ~ net_talent_dm - 1, data = p)
  eff_mod  <- lm(eff_dm ~ net_talent_dm - 1, data = p)

  # Also run with factor dummies for comparison
  wins_full <- lm(wins ~ net_talent + factor(team_id) + factor(season),
                   data = panel)
  eff_full  <- lm(adj_efficiency ~ net_talent + factor(team_id) + factor(season),
                   data = panel)

  list(wins = wins_mod, efficiency = eff_mod,
       wins_full = wins_full, efficiency_full = eff_full)
}

# ── 3. Two-Stage Least Squares (Bartik IV) ────────────────────────────────────
# Manual 2SLS implementation:
#   Stage 1: net_talent = alpha + gamma * bartik_instrument + team_FE + season_FE + u
#   Stage 2: wins = beta_0 + beta_1 * net_talent_hat + team_FE + season_FE + e
#
# Correct 2SLS SEs use actual (not predicted) residuals.

run_2sls <- function(panel, instrument = "bartik_talent") {
  p <- panel

  # Demean everything (within-transformation for two-way FE)
  p$wins_dm    <- demean(p$wins, p$team_id) |> demean(p$season)
  p$eff_dm     <- demean(p$adj_efficiency, p$team_id) |> demean(p$season)
  p$talent_dm  <- demean(p$net_talent, p$team_id) |> demean(p$season)
  p$iv_dm      <- demean(p[[instrument]], p$team_id) |> demean(p$season)

  # ── First stage ──
  first_stage <- lm(talent_dm ~ iv_dm - 1, data = p)
  p$talent_hat <- fitted(first_stage)

  # First-stage F-stat
  fs_summary <- summary(first_stage)
  f_stat <- fs_summary$fstatistic
  if (!is.null(f_stat)) {
    f_val <- f_stat[1]
  } else {
    # Manual F-stat for no-intercept model
    ss_model <- sum(p$talent_hat^2)
    ss_resid <- sum(residuals(first_stage)^2)
    df_model <- 1
    df_resid <- nrow(p) - 1
    f_val <- (ss_model / df_model) / (ss_resid / df_resid)
  }

  # ── Second stage ──
  second_stage_wins <- lm(wins_dm ~ talent_hat - 1, data = p)
  second_stage_eff  <- lm(eff_dm ~ talent_hat - 1, data = p)

  # Correct 2SLS standard errors
  # Use actual residuals: y - X * beta_2sls (where X is ACTUAL, not predicted)
  correct_2sls_se <- function(second_model, y_dm, x_dm, x_hat) {
    beta_iv <- coef(second_model)
    actual_resid <- y_dm - x_dm * beta_iv
    n <- length(y_dm)
    sigma2 <- sum(actual_resid^2) / (n - 1)
    se <- sqrt(sigma2 / sum(x_hat^2))
    se
  }

  se_wins <- correct_2sls_se(second_stage_wins, p$wins_dm, p$talent_dm, p$talent_hat)
  se_eff  <- correct_2sls_se(second_stage_eff, p$eff_dm, p$talent_dm, p$talent_hat)

  list(
    first_stage = first_stage,
    second_stage_wins = second_stage_wins,
    second_stage_eff  = second_stage_eff,
    f_stat = f_val,
    iv_coef_wins = coef(second_stage_wins)[1],
    iv_se_wins   = se_wins,
    iv_coef_eff  = coef(second_stage_eff)[1],
    iv_se_eff    = se_eff,
    demeaned_data = p
  )
}

# ── 4. Reduced form ──────────────────────────────────────────────────────────
run_reduced_form <- function(panel) {
  p <- panel
  p$wins_dm <- demean(p$wins, p$team_id) |> demean(p$season)
  p$eff_dm  <- demean(p$adj_efficiency, p$team_id) |> demean(p$season)
  p$iv_dm   <- demean(p$bartik_talent, p$team_id) |> demean(p$season)

  list(
    wins = lm(wins_dm ~ iv_dm - 1, data = p),
    efficiency = lm(eff_dm ~ iv_dm - 1, data = p)
  )
}

# ── 5. Difference-in-Differences ─────────────────────────────────────────────
run_did <- function(panel) {
  p <- panel[order(panel$team_id, panel$season), ]
  p$treated <- as.integer(p$n_coaching_transfers > 0)

  # Add lagged outcomes
  p$lag_wins <- ave(p$wins, p$team_id,
                    FUN = function(x) c(NA, head(x, -1)))
  p$lag_eff  <- ave(p$adj_efficiency, p$team_id,
                    FUN = function(x) c(NA, head(x, -1)))
  p$delta_wins <- p$wins - p$lag_wins
  p$delta_eff  <- p$adj_efficiency - p$lag_eff

  did_data <- p[!is.na(p$lag_wins), ]

  # First-difference with season FE
  fd_wins <- lm(delta_wins ~ treated + factor(season), data = did_data)
  fd_eff  <- lm(delta_eff ~ treated + factor(season), data = did_data)

  # TWFE
  twfe_wins <- lm(wins ~ treated + factor(team_id) + factor(season), data = did_data)
  twfe_eff  <- lm(adj_efficiency ~ treated + factor(team_id) + factor(season),
                   data = did_data)

  list(
    fd_wins = fd_wins, fd_eff = fd_eff,
    twfe_wins = twfe_wins, twfe_eff = twfe_eff,
    did_data = did_data
  )
}

# ── 6. Full analysis runner ──────────────────────────────────────────────────

run_all_specifications <- function(panel, true_beta = NULL) {

  cat("================================================================\n")
  cat("  COLLEGE BASKETBALL TRANSFER PORTAL - CAUSAL ANALYSIS RESULTS\n")
  if (!is.null(true_beta)) {
    cat(sprintf("  True causal effect (DGP): %.2f wins per unit net talent\n",
                true_beta))
  }
  cat("================================================================\n\n")

  # ── Run all specifications ──
  naive <- run_naive_ols(panel)
  fe    <- run_fe_ols(panel)
  iv    <- run_2sls(panel, "bartik_talent")
  rf    <- run_reduced_form(panel)
  did   <- run_did(panel)

  # ── First stage ──
  cat("-------------------------------------------------------\n")
  cat("  FIRST STAGE: Bartik Instrument -> Net Transfer Talent\n")
  cat("-------------------------------------------------------\n")
  cat(sprintf("  First-stage F-statistic: %.1f\n", iv$f_stat))
  cat("  (Rule of thumb: F > 10 for strong instrument)\n")
  print_coef_table(iv$first_stage, label = "First Stage Regression")

  # ── Main results: Wins equation ──
  cat("\n\n-------------------------------------------------------\n")
  cat("  WINS EQUATION: Comparing Identification Strategies\n")
  cat("-------------------------------------------------------\n")

  cat("\n  [1] Naive OLS:\n")
  print_coef_table(naive$wins)

  cat("\n  [2] Team + Season Fixed Effects:\n")
  print_coef_table(fe$wins, label = "FE (demeaned)")

  cat(sprintf("\n  [3] Bartik IV (2SLS):\n"))
  cat(sprintf("      Coefficient: %.4f\n", iv$iv_coef_wins))
  cat(sprintf("      Std. Error:  %.4f\n", iv$iv_se_wins))
  cat(sprintf("      t-statistic: %.3f\n", iv$iv_coef_wins / iv$iv_se_wins))
  cat(sprintf("      p-value:     %.4f\n",
              2 * pnorm(-abs(iv$iv_coef_wins / iv$iv_se_wins))))

  # ── Efficiency equation ──
  cat("\n\n-------------------------------------------------------\n")
  cat("  EFFICIENCY EQUATION: Comparing Strategies\n")
  cat("-------------------------------------------------------\n")

  cat("\n  [1] Naive OLS:\n")
  print_coef_table(naive$efficiency)

  cat("\n  [2] Team + Season Fixed Effects:\n")
  print_coef_table(fe$efficiency, label = "FE (demeaned)")

  cat(sprintf("\n  [3] Bartik IV (2SLS):\n"))
  cat(sprintf("      Coefficient: %.4f\n", iv$iv_coef_eff))
  cat(sprintf("      Std. Error:  %.4f\n", iv$iv_se_eff))

  # ── DiD ──
  cat("\n\n-------------------------------------------------------\n")
  cat("  DIFFERENCE-IN-DIFFERENCES (Coaching Change Treatment)\n")
  cat("-------------------------------------------------------\n")

  cat("\n  First-Difference (delta_wins ~ treated + season FE):\n")
  # Only print the treated coefficient
  cf <- coef(did$fd_wins)["treated"]
  se <- robust_se(did$fd_wins)[names(cf)]
  cat(sprintf("      treated: %.4f (SE: %.4f, t: %.3f)\n",
              cf, se, cf/se))

  cat("\n  TWFE (wins ~ treated + team_FE + season_FE):\n")
  cf_twfe <- coef(did$twfe_wins)["treated"]
  se_twfe <- summary(did$twfe_wins)$coefficients["treated", 2]
  cat(sprintf("      treated: %.4f (SE: %.4f, t: %.3f)\n",
              cf_twfe, se_twfe, cf_twfe/se_twfe))

  # ── Summary comparison ──
  cat("\n\n=======================================================\n")
  cat("  COEFFICIENT COMPARISON (Effect on Wins)\n")
  cat("=======================================================\n")
  naive_coef <- coef(naive$wins)["net_talent"]
  fe_coef    <- coef(fe$wins)["net_talent_dm"]
  iv_coef    <- iv$iv_coef_wins

  cat(sprintf("  %-25s  %8.4f\n", "Naive OLS:", naive_coef))
  cat(sprintf("  %-25s  %8.4f\n", "Team+Season FE:", fe_coef))
  cat(sprintf("  %-25s  %8.4f\n", "Bartik IV (2SLS):", iv_coef))
  if (!is.null(true_beta)) {
    cat(sprintf("  %-25s  %8.4f\n", "TRUE DGP beta:", true_beta))
  }
  cat("=======================================================\n")

  list(
    naive = naive, fe = fe, iv = iv, rf = rf, did = did,
    summary = data.frame(
      method     = c("Naive OLS", "Team+Season FE", "Bartik IV"),
      coef_wins  = c(naive_coef, fe_coef, iv_coef),
      se_wins    = c(
        summary(naive$wins)$coefficients["net_talent", 2],
        robust_se(fe$wins)["net_talent_dm"],
        iv$iv_se_wins
      )
    )
  )
}

# ── 7. Sensitivity: multiple instruments ──────────────────────────────────────
run_sensitivity <- function(panel) {
  cat("\n-------------------------------------------------------\n")
  cat("  SENSITIVITY: Alternative IV Constructions\n")
  cat("-------------------------------------------------------\n\n")

  iv_talent <- run_2sls(panel, "bartik_talent")
  iv_count  <- run_2sls(panel, "bartik_count")

  cat(sprintf("  IV (talent-weighted):  beta = %.4f (SE = %.4f, F = %.1f)\n",
              iv_talent$iv_coef_wins, iv_talent$iv_se_wins, iv_talent$f_stat))
  cat(sprintf("  IV (count-weighted):   beta = %.4f (SE = %.4f, F = %.1f)\n",
              iv_count$iv_coef_wins, iv_count$iv_se_wins, iv_count$f_stat))

  list(iv_talent = iv_talent, iv_count = iv_count)
}
