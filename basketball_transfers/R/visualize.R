###############################################################################
# visualize.R
# Visualization functions for transfer portal causal analysis
# (Base R graphics only — no external dependencies)
###############################################################################

# ── 1. Transfer market overview ───────────────────────────────────────────────
plot_transfer_market <- function(sim, output_dir = "output") {
  transfers <- sim$transfers

  filepath <- file.path(output_dir, "01_transfer_market.png")
  png(filepath, width = 1400, height = 1000, res = 150)

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  # Panel 1: Volume by season
  vol <- table(transfers$season)
  barplot(vol, col = "#2166AC", border = NA,
          main = "Transfer Portal Volume by Season",
          xlab = "Season", ylab = "Number of Transfers")

  # Panel 2: Talent distribution by position
  positions <- c("PG", "SG", "SF", "PF", "C")
  cols <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
  xlim <- range(transfers$player_talent)
  plot(NULL, xlim = xlim, ylim = c(0, 0.5),
       main = "Talent Distribution by Position",
       xlab = "Player Talent", ylab = "Density")
  for (i in seq_along(positions)) {
    x <- transfers$player_talent[transfers$position == positions[i]]
    if (length(x) > 2) {
      d <- density(x, adjust = 1.2)
      lines(d$x, d$y, col = cols[i], lwd = 2)
    }
  }
  legend("topright", positions, col = cols, lwd = 2, cex = 0.8, bty = "n")

  # Panel 3: Coaching change transfers vs. others
  cc  <- transfers$player_talent[transfers$coaching_change_transfer == 1]
  oth <- transfers$player_talent[transfers$coaching_change_transfer == 0]
  if (length(cc) > 2 && length(oth) > 2) {
    d_cc  <- density(cc, adjust = 1.2)
    d_oth <- density(oth, adjust = 1.2)
    xlim2 <- range(c(d_cc$x, d_oth$x))
    ylim2 <- range(c(d_cc$y, d_oth$y))
    plot(d_oth$x, d_oth$y, type = "l", col = "#2166AC", lwd = 2,
         xlim = xlim2, ylim = ylim2,
         main = "Talent by Portal Entry Reason",
         xlab = "Player Talent", ylab = "Density")
    lines(d_cc$x, d_cc$y, col = "#B2182B", lwd = 2)
    legend("topright", c("Other", "Coaching Change"),
           col = c("#2166AC", "#B2182B"), lwd = 2, cex = 0.8, bty = "n")
  }

  # Panel 4: Net talent distribution by conference
  panel <- sim$team_panel
  power_confs <- c("ACC", "Big12", "BigTen", "BigEast", "SEC", "Pac12")
  panel$conf_type <- ifelse(panel$conference %in% power_confs, "Power", "Mid-Major")
  boxplot(net_talent ~ conf_type, data = panel,
          col = c("#8da0cb", "#fc8d62"),
          main = "Net Transfer Talent: Power vs. Mid-Major",
          ylab = "Net Transfer Talent")

  dev.off()
  cat(sprintf("  Saved: %s\n", filepath))
}

# ── 2. First stage visualization ──────────────────────────────────────────────
plot_first_stage <- function(sim, output_dir = "output") {
  panel <- sim$team_panel
  p <- panel[panel$bartik_talent != 0, ]

  if (nrow(p) < 20) {
    cat("  Skipping first stage plot: insufficient non-zero instrument values\n")
    return(invisible(NULL))
  }

  # Create bins
  n_bins <- min(20, floor(nrow(p) / 5))
  p$bin <- cut(p$bartik_talent, breaks = n_bins, labels = FALSE)
  bin_means <- aggregate(cbind(bartik_talent, net_talent) ~ bin, data = p, FUN = mean)
  bin_se <- aggregate(net_talent ~ bin, data = p,
                      FUN = function(x) sd(x) / sqrt(length(x)))
  bin_means$se <- bin_se$net_talent

  filepath <- file.path(output_dir, "02_first_stage.png")
  png(filepath, width = 1000, height = 700, res = 150)

  par(mar = c(5, 5, 4, 2))
  plot(bin_means$bartik_talent, bin_means$net_talent,
       pch = 19, col = "#2166AC", cex = 1.5,
       xlab = "Bartik Instrument (Predicted Talent from Coaching-Change Shocks)",
       ylab = "Actual Net Transfer Talent",
       main = "First Stage: Bartik Instrument Predicts Net Transfer Talent")
  arrows(bin_means$bartik_talent,
         bin_means$net_talent - 1.96 * bin_means$se,
         bin_means$bartik_talent,
         bin_means$net_talent + 1.96 * bin_means$se,
         code = 3, angle = 90, length = 0.05, col = "#2166AC80")
  fit <- lm(net_talent ~ bartik_talent, data = bin_means)
  abline(fit, col = "#B2182B", lwd = 2)
  mtext("Binned scatter plot with OLS fit", side = 3, line = 0.3, cex = 0.8, col = "grey40")

  dev.off()
  cat(sprintf("  Saved: %s\n", filepath))
}

# ── 3. Reduced form visualization ────────────────────────────────────────────
plot_reduced_form <- function(sim, output_dir = "output") {
  panel <- sim$team_panel
  p <- panel[panel$bartik_talent != 0, ]

  if (nrow(p) < 20) {
    cat("  Skipping reduced form plot: insufficient data\n")
    return(invisible(NULL))
  }

  n_bins <- min(20, floor(nrow(p) / 5))
  p$bin <- cut(p$bartik_talent, breaks = n_bins, labels = FALSE)

  filepath <- file.path(output_dir, "03_reduced_form.png")
  png(filepath, width = 1400, height = 700, res = 150)
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

  # Wins
  bin_w <- aggregate(cbind(bartik_talent, wins) ~ bin, data = p, FUN = mean)
  bin_w$se <- aggregate(wins ~ bin, data = p,
                        FUN = function(x) sd(x)/sqrt(length(x)))$wins
  plot(bin_w$bartik_talent, bin_w$wins,
       pch = 19, col = "#2166AC", cex = 1.5,
       xlab = "Bartik Instrument", ylab = "Wins",
       main = "Reduced Form: Instrument -> Wins")
  arrows(bin_w$bartik_talent, bin_w$wins - 1.96 * bin_w$se,
         bin_w$bartik_talent, bin_w$wins + 1.96 * bin_w$se,
         code = 3, angle = 90, length = 0.05, col = "#2166AC80")
  abline(lm(wins ~ bartik_talent, data = bin_w), col = "#B2182B", lwd = 2)

  # Efficiency
  bin_e <- aggregate(cbind(bartik_talent, adj_efficiency) ~ bin, data = p, FUN = mean)
  bin_e$se <- aggregate(adj_efficiency ~ bin, data = p,
                        FUN = function(x) sd(x)/sqrt(length(x)))$adj_efficiency
  plot(bin_e$bartik_talent, bin_e$adj_efficiency,
       pch = 19, col = "#2166AC", cex = 1.5,
       xlab = "Bartik Instrument", ylab = "Adj. Efficiency",
       main = "Reduced Form: Instrument -> Efficiency")
  arrows(bin_e$bartik_talent, bin_e$adj_efficiency - 1.96 * bin_e$se,
         bin_e$bartik_talent, bin_e$adj_efficiency + 1.96 * bin_e$se,
         code = 3, angle = 90, length = 0.05, col = "#2166AC80")
  abline(lm(adj_efficiency ~ bartik_talent, data = bin_e), col = "#B2182B", lwd = 2)

  dev.off()
  cat(sprintf("  Saved: %s\n", filepath))
}

# ── 4. OLS vs IV coefficient comparison ──────────────────────────────────────
plot_ols_vs_iv <- function(results, true_beta = NULL, output_dir = "output") {
  specs <- results$summary

  filepath <- file.path(output_dir, "04_ols_vs_iv.png")
  png(filepath, width = 900, height = 700, res = 150)

  par(mar = c(5, 10, 4, 3))
  ypos <- barplot(specs$coef_wins, horiz = TRUE,
                  names.arg = specs$method, las = 1,
                  col = c("#fc8d62", "#8da0cb", "#2166AC"),
                  border = NA, xlim = range(c(0,
                    specs$coef_wins + 2 * specs$se_wins,
                    if (!is.null(true_beta)) true_beta + 0.5 else 0)),
                  main = "Effect of Net Transfer Talent on Wins",
                  xlab = "Estimated Effect (Wins per Unit Net Talent)")

  # Error bars
  arrows(specs$coef_wins - 1.96 * specs$se_wins, ypos,
         specs$coef_wins + 1.96 * specs$se_wins, ypos,
         code = 3, angle = 90, length = 0.1, lwd = 2)

  # True beta reference line
  if (!is.null(true_beta)) {
    abline(v = true_beta, lty = 2, col = "#B2182B", lwd = 2)
    text(true_beta, max(ypos) + 0.5,
         paste("True beta =", true_beta),
         col = "#B2182B", cex = 0.8, font = 3)
  }

  abline(v = 0, lty = 3, col = "grey50")

  dev.off()
  cat(sprintf("  Saved: %s\n", filepath))
}

# ── 5. Event study plot ──────────────────────────────────────────────────────
plot_event_study <- function(sim, output_dir = "output") {
  panel <- sim$team_panel

  # Identify first treatment season for each team
  treated_teams <- panel[panel$n_coaching_transfers > 0, ]
  if (nrow(treated_teams) == 0) {
    cat("  Skipping event study: no coaching-change transfers\n")
    return(invisible(NULL))
  }

  first_treat <- aggregate(season ~ team_id, data = treated_teams, FUN = min)
  names(first_treat)[2] <- "first_treat_season"

  # Build event-time data
  event_data <- merge(panel, first_treat, by = "team_id")
  event_data$event_time <- event_data$season - event_data$first_treat_season
  event_data <- event_data[event_data$event_time >= -3 & event_data$event_time <= 3, ]

  # Control group average
  never_treated <- panel[!panel$team_id %in% first_treat$team_id, ]
  control_mean <- mean(never_treated$wins, na.rm = TRUE)

  # Event-time means
  event_means <- aggregate(wins ~ event_time, data = event_data, FUN = mean)
  event_se    <- aggregate(wins ~ event_time, data = event_data,
                           FUN = function(x) sd(x)/sqrt(length(x)))

  filepath <- file.path(output_dir, "05_event_study.png")
  png(filepath, width = 1000, height = 700, res = 150)

  par(mar = c(5, 5, 4, 2))
  ylim <- range(c(event_means$wins - 2 * event_se$wins,
                   event_means$wins + 2 * event_se$wins,
                   control_mean))

  plot(event_means$event_time, event_means$wins,
       type = "b", pch = 19, col = "#2166AC", lwd = 2, cex = 1.5,
       xlim = c(-3.5, 3.5), ylim = ylim,
       xlab = "Seasons Relative to First Coaching-Change Transfer",
       ylab = "Average Wins",
       main = "Event Study: Wins Around Coaching-Change Transfer Receipt",
       xaxt = "n")
  axis(1, at = -3:3)

  # CI ribbon
  polygon(c(event_means$event_time, rev(event_means$event_time)),
          c(event_means$wins - 1.96 * event_se$wins,
            rev(event_means$wins + 1.96 * event_se$wins)),
          col = "#2166AC20", border = NA)

  # Reference lines
  abline(v = -0.5, lty = 2, col = "grey60")
  abline(h = control_mean, lty = 3, col = "#B2182B")
  text(-2.5, control_mean + 0.3, "Never-treated mean",
       col = "#B2182B", cex = 0.7, font = 3)

  dev.off()
  cat(sprintf("  Saved: %s\n", filepath))
}

# ── 6. Conference heterogeneity ───────────────────────────────────────────────
plot_conference_heterogeneity <- function(sim, output_dir = "output") {
  panel <- sim$team_panel
  confs <- unique(panel$conference)

  # Estimate effect by conference
  conf_effects <- data.frame(
    conference = character(),
    estimate = numeric(),
    se = numeric(),
    stringsAsFactors = FALSE
  )

  for (conf in confs) {
    d <- panel[panel$conference == conf, ]
    if (nrow(d) < 15) next
    mod <- lm(wins ~ net_talent + program_strength, data = d)
    cf <- coef(mod)["net_talent"]
    se <- summary(mod)$coefficients["net_talent", 2]
    conf_effects <- rbind(conf_effects,
                          data.frame(conference = conf, estimate = cf, se = se))
  }

  conf_effects <- conf_effects[order(conf_effects$estimate), ]

  filepath <- file.path(output_dir, "06_conference_heterogeneity.png")
  png(filepath, width = 1000, height = 700, res = 150)

  par(mar = c(5, 8, 4, 2))
  n <- nrow(conf_effects)
  ypos <- 1:n

  plot(conf_effects$estimate, ypos,
       pch = 19, col = "#2166AC", cex = 1.5,
       xlim = range(c(conf_effects$estimate - 2 * conf_effects$se,
                       conf_effects$estimate + 2 * conf_effects$se)),
       yaxt = "n", xlab = "Effect on Wins", ylab = "",
       main = "Transfer Talent Effect by Conference")
  axis(2, at = ypos, labels = conf_effects$conference, las = 1, cex.axis = 0.8)
  arrows(conf_effects$estimate - 1.96 * conf_effects$se, ypos,
         conf_effects$estimate + 1.96 * conf_effects$se, ypos,
         code = 3, angle = 90, length = 0.05, col = "#2166AC")
  abline(v = 0, lty = 2, col = "grey50")

  dev.off()
  cat(sprintf("  Saved: %s\n", filepath))
}

# ── 7. Master plotting function ───────────────────────────────────────────────
generate_all_plots <- function(sim, results, output_dir = "output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Generating plots...\n")

  plot_transfer_market(sim, output_dir)
  plot_first_stage(sim, output_dir)
  plot_reduced_form(sim, output_dir)
  plot_ols_vs_iv(results, true_beta = sim$true_beta, output_dir = output_dir)
  plot_event_study(sim, output_dir)
  plot_conference_heterogeneity(sim, output_dir)

  cat(sprintf("\nAll plots saved to %s/\n", output_dir))
}
