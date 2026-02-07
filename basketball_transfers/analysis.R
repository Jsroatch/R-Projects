###############################################################################
# analysis.R -- College Basketball Transfer Portal Causal Analysis
# (Base R only -- no external package dependencies)
#
# RESEARCH QUESTION:
#   What is the causal effect of acquiring transfer portal talent on
#   college basketball team performance?
#
# IDENTIFICATION PROBLEM:
#   Naive regression of wins on transfer talent is biased: strong programs
#   attract better transfers AND win more. We need exogenous variation in
#   which transfers end up where.
#
# IDENTIFICATION STRATEGY:
#   Shift-share (Bartik) instrumental variables.
#   - SHARES: Each team's pre-determined recruiting patterns (position needs,
#     geographic pipelines, conference ties) predict which types of transfers
#     they pursue. Measured BEFORE the transfer window opens.
#   - SHIFTS: Coaching changes at OTHER programs cause unexpected portal
#     entries, creating supply shocks to specific position x conference cells.
#   - INSTRUMENT: The interaction of pre-determined shares with exogenous
#     supply shifts generates plausibly exogenous variation in the transfer
#     talent a team actually receives.
#
#   Exclusion restriction: A coaching change at School X affects School Y's
#   wins ONLY through the transfer players School X releases into the portal
#   (conditional on School Y's fixed effects and season trends).
#
# SUPPLEMENTARY STRATEGY:
#   Difference-in-differences comparing teams that received coaching-change
#   driven transfers vs. teams that didn't, with event-study plots to
#   verify parallel pre-trends.
#
# DATA:
#   Simulated to match key features of the real transfer market. When real
#   data is available (e.g., from barttorvik.com, 247sports transfer database,
#   or the NCAA transfer portal API), replace simulate_transfer_data() below.
#
# USAGE:
#   source("analysis.R")  # Runs everything
#   # Or interactively:
#   source("R/simulate_data.R")
#   source("R/identification.R")
#   source("R/visualize.R")
#   sim     <- simulate_transfer_data()
#   results <- run_all_specifications(sim$team_panel, true_beta = sim$true_beta)
###############################################################################

# -- Setup -------------------------------------------------------------------
cat("
================================================================
  College Basketball Transfer Portal -- Causal Analysis Pipeline
  Exploiting Quasi-Random Variation in Transfer Moves
================================================================
\n")

# Source modules
source("R/simulate_data.R")
source("R/identification.R")
source("R/visualize.R")

# -- 1. Generate (or load) data ---------------------------------------------
cat("-- Step 1: Simulating transfer portal data --------------------------\n")
cat("   (Replace with your own data loader when actual data is available)\n\n")

sim <- simulate_transfer_data(
  n_teams   = 120,    # ~120 D1 programs with meaningful transfer activity
  n_seasons = 6,      # 6 seasons of portal data
  seed      = 42
)

cat(sprintf("   Generated %d team-seasons, %d individual transfers\n",
            nrow(sim$team_panel), nrow(sim$transfers)))
cat(sprintf("   True causal effect (DGP): beta = %.1f wins per unit net talent\n\n",
            sim$true_beta))

# -- 2. Descriptive statistics -----------------------------------------------
cat("-- Step 2: Descriptive statistics -----------------------------------\n\n")

panel <- sim$team_panel

desc_vars <- c("wins", "adj_efficiency", "net_talent", "total_talent_in",
               "n_transfers_in", "bartik_talent")
desc_stats <- do.call(rbind, lapply(desc_vars, function(v) {
  x <- panel[[v]]
  data.frame(
    variable = v,
    mean = round(mean(x, na.rm = TRUE), 2),
    sd   = round(sd(x, na.rm = TRUE), 2),
    min  = round(min(x, na.rm = TRUE), 2),
    max  = round(max(x, na.rm = TRUE), 2)
  )
}))
print(desc_stats, row.names = FALSE)
cat("\n")

# -- 3. Run all econometric specifications -----------------------------------
cat("-- Step 3: Econometric analysis -------------------------------------\n\n")

results <- run_all_specifications(panel, true_beta = sim$true_beta)

# -- 4. Sensitivity analysis -------------------------------------------------
cat("\n-- Step 4: Sensitivity analysis --------------------------------------\n")

sensitivity <- run_sensitivity(panel)

# -- 5. Subgroup analysis: power vs. mid-major -------------------------------
cat("\n-- Step 5: Heterogeneity -- Power vs. Mid-Major ----------------------\n\n")

power_panel <- panel[panel$power_conf == 1, ]
mid_panel   <- panel[panel$power_conf == 0, ]

iv_power <- run_2sls(power_panel, "bartik_talent")
iv_mid   <- run_2sls(mid_panel, "bartik_talent")

cat(sprintf("  Power conferences:  beta = %.4f (SE = %.4f, F = %.1f)\n",
            iv_power$iv_coef_wins, iv_power$iv_se_wins, iv_power$f_stat))
cat(sprintf("  Mid-major:          beta = %.4f (SE = %.4f, F = %.1f)\n",
            iv_mid$iv_coef_wins, iv_mid$iv_se_wins, iv_mid$f_stat))

# -- 6. Position-specific effects --------------------------------------------
cat("\n-- Step 6: Position-specific transfer effects -----------------------\n\n")

positions <- c("PG", "SG", "SF", "PF", "C")
transfers <- sim$transfers

# Aggregate talent by position
for (pos in positions) {
  pos_trans <- transfers[transfers$position == pos, ]
  if (nrow(pos_trans) == 0) next
  pos_agg <- aggregate(player_talent ~ dest_team_id + season,
                       data = pos_trans, FUN = sum)
  names(pos_agg)[3] <- paste0("talent_", pos)
  panel <- merge(panel, pos_agg,
                 by.x = c("team_id", "season"),
                 by.y = c("dest_team_id", "season"),
                 all.x = TRUE)
  panel[[paste0("talent_", pos)]][is.na(panel[[paste0("talent_", pos)]])] <- 0
}

pos_formula <- as.formula(paste("wins ~",
  paste0("talent_", positions, collapse = " + "),
  "+ program_strength + factor(team_id) + factor(season)"))

pos_model <- lm(pos_formula, data = panel)

cat("Position-specific effects on wins (with team + season FE):\n")
pos_coefs <- coef(pos_model)[paste0("talent_", positions)]
pos_ses   <- summary(pos_model)$coefficients[paste0("talent_", positions), 2]
pos_table <- data.frame(
  Position  = positions,
  Estimate  = round(pos_coefs, 3),
  Std.Error = round(pos_ses, 3),
  t.value   = round(pos_coefs / pos_ses, 2)
)
print(pos_table, row.names = FALSE)

# -- 7. Generate visualizations ---------------------------------------------
cat("\n-- Step 7: Generating visualizations ---------------------------------\n\n")

output_dir <- "output"
generate_all_plots(sim, results, output_dir = output_dir)

# -- 8. Summary --------------------------------------------------------------
cat("\n")
cat("================================================================\n")
cat("  SUMMARY\n")
cat("================================================================\n\n")

naive_coef <- results$summary$coef_wins[1]
fe_coef    <- results$summary$coef_wins[2]
iv_coef    <- results$summary$coef_wins[3]

cat(sprintf("  True DGP effect:    beta = %.2f\n", sim$true_beta))
cat(sprintf("  Naive OLS:          beta = %.2f  (biased -- conflates talent with quality)\n", naive_coef))
cat(sprintf("  Team+Season FE:     beta = %.2f  (better -- controls for fixed program effects)\n", fe_coef))
cat(sprintf("  Bartik IV:          beta = %.2f  (exploits quasi-random coaching-change shocks)\n", iv_coef))
cat(sprintf("\n  IV estimate is %.1f%% of true effect",
            100 * iv_coef / sim$true_beta))
cat(sprintf(" (OLS bias: %.1f%%)\n", 100 * (naive_coef - sim$true_beta) / sim$true_beta))
cat("\n  Key insight: Naive OLS overstates the effect because it attributes\n")
cat("  program-quality advantages to transfer talent. The Bartik IV, which\n")
cat("  isolates variation from coaching changes at OTHER schools, recovers\n")
cat("  an estimate closer to the true causal effect.\n\n")

cat("  Plots saved to: ", normalizePath(output_dir, mustWork = FALSE), "\n")
cat("================================================================\n")
