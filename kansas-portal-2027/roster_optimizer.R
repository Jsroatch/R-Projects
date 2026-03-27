# =============================================================================
# Kansas Basketball 2026-27 — Roster Optimization Model
# Optimizes top-6/7 man rotation for team efficiency within NIL budget
#
# Approach:
#   Maximize composite "Team Efficiency Score" (TES) subject to:
#     1. Position constraints (Hi-Lo requires at least 1 true C/PF)
#     2. Budget constraint (NIL salary cap)
#     3. Minimum defensive rating threshold (Self won't play guys who can't guard)
#     4. Roster balance (can't stack 5 bigs or 5 guards)
#
# Team Efficiency Score (TES) formula — weights tuned to Self's system priorities:
#   TES_i = 0.25*(ortg_norm) + 0.30*(drtg_norm_inv) + 0.15*(ts_norm)
#         + 0.15*(orb_norm) + 0.10*(bpm_norm) + 0.05*(fg3_norm)
#   where _norm = min-max normalized across candidate pool
# =============================================================================

source("player_data.R")

library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# Build unified candidate pools per scenario
# -----------------------------------------------------------------------------

build_candidate_pool <- function(scenario = "A") {

  if (scenario == "A") {
    base_players <- bind_rows(
      kansas_returning %>% mutate(type = "returning", certainty = "confirmed"),
      kansas_freshmen  %>% mutate(type = "freshman")
    ) %>% mutate(nir_sal_est = nir_sal, self_fit = 5,
                 value_tier = "committed", notes = "On roster")
  } else {
    base_players <- bind_rows(
      kansas_returning %>%
        filter(name != "Flory Bidunga") %>%
        mutate(type = "returning", certainty = "confirmed"),
      kansas_freshmen %>% mutate(type = "freshman")
    ) %>% mutate(nir_sal_est = nir_sal, self_fit = 5,
                 value_tier = "committed", notes = "On roster")
  }

  base_players <- base_players %>%
    select(name, pos, ht, ortg, drtg, bpm, obpm, dbpm, usage,
           orb_pct, fg3_pct, fg3_rate, ts_pct, stl_pct, blk_pct,
           nir_sal_est, value_tier, self_fit, notes, type)

  portal_clean <- portal_targets %>%
    mutate(type = "portal") %>%
    select(name, pos, ht, ortg, drtg, bpm, obpm, dbpm, usage,
           orb_pct, fg3_pct, fg3_rate, ts_pct, stl_pct, blk_pct,
           nir_sal_est, value_tier, self_fit, notes, type)

  bind_rows(base_players, portal_clean)
}


# -----------------------------------------------------------------------------
# Normalize metrics for TES calculation
# -----------------------------------------------------------------------------

normalize_col <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}

compute_tes <- function(df) {
  df %>%
    mutate(
      ortg_n    = normalize_col(ortg),
      drtg_n    = normalize_col(-drtg),   # lower drtg = better defense
      ts_n      = normalize_col(ts_pct),
      orb_n     = normalize_col(orb_pct),
      bpm_n     = normalize_col(bpm),
      fg3_n     = normalize_col(fg3_pct),
      self_fit_n = normalize_col(self_fit),
      # Weighted composite — Self's system priorities
      TES = 0.25 * ortg_n +
            0.30 * drtg_n +
            0.15 * ts_n   +
            0.15 * orb_n  +
            0.10 * bpm_n  +
            0.05 * fg3_n
    )
}

# -----------------------------------------------------------------------------
# Greedy optimizer: pick best TES combo within budget + position constraints
# Uses exhaustive search over portal targets (feasible given small N)
# -----------------------------------------------------------------------------

optimize_roster <- function(scenario = "A",
                            roster_size = 7,
                            min_c_pf = 1,    # must have at least 1 true big
                            max_guards = 3,  # no more than 3 G/PG/SG
                            drtg_threshold = 103,  # Self cuts guys above this
                            print_results = TRUE) {

  pool <- build_candidate_pool(scenario)
  budget <- ifelse(scenario == "A",
                   budget_params$scenario_a_total,
                   budget_params$scenario_b_total)

  # Apply defensive threshold filter (Self won't play non-defenders)
  pool_filtered <- pool %>%
    filter(drtg <= drtg_threshold | type == "committed" | type == "returning" | type == "freshman")

  # Score everyone
  pool_scored <- compute_tes(pool_filtered)

  # Committed players are always on the roster
  committed <- pool_scored %>% filter(type %in% c("returning", "freshman"))
  portal_pool <- pool_scored %>% filter(type == "portal")

  committed_cost <- sum(committed$nir_sal_est)
  remaining_budget <- budget - committed_cost

  if (print_results) {
    message("\n=== SCENARIO ", scenario, " ===")
    message("Total budget: $", format(budget, big.mark=","))
    message("Committed cost: $", format(committed_cost, big.mark=","))
    message("Remaining portal budget: $", format(remaining_budget, big.mark=","))
  }

  portal_slots <- roster_size - nrow(committed)

  # Exhaustive search over all combinations of portal_slots players
  portal_combos <- combn(nrow(portal_pool), portal_slots, simplify = FALSE)

  best_tes   <- -Inf
  best_combo <- NULL

  for (idx in portal_combos) {
    picks <- portal_pool[idx, ]

    # Budget check
    if (sum(picks$nir_sal_est) > remaining_budget) next

    # Position check — need at least min_c_pf bigs (C or PF)
    roster_all <- bind_rows(committed, picks)
    n_bigs   <- sum(roster_all$pos %in% c("C", "PF", "PF/C"))
    n_guards <- sum(roster_all$pos %in% c("PG", "SG", "G"))

    if (n_bigs < min_c_pf) next
    if (n_guards > max_guards) next

    avg_tes <- mean(bind_rows(committed, picks)$TES)
    if (avg_tes > best_tes) {
      best_tes   <- avg_tes
      best_combo <- picks
    }
  }

  if (is.null(best_combo)) {
    message("No feasible roster found — relax constraints or budget.")
    return(NULL)
  }

  final_roster <- bind_rows(committed, best_combo) %>%
    arrange(desc(TES)) %>%
    select(name, pos, ht, ortg, drtg, bpm, ts_pct, orb_pct,
           fg3_pct, nir_sal_est, TES, value_tier, type, notes)

  if (print_results) {
    message("\n--- Optimal ", roster_size, "-man Rotation (TES = ",
            round(best_tes, 3), ") ---")
    print(final_roster %>%
            select(name, pos, ortg, drtg, bpm, ts_pct, nir_sal_est, TES, value_tier))
    message("\nTotal NIL Cost: $", format(sum(final_roster$nir_sal_est), big.mark=","))
    message("Budget Remaining: $",
            format(budget - sum(final_roster$nir_sal_est), big.mark=","))
  }

  invisible(final_roster)
}

# -----------------------------------------------------------------------------
# Team-level projected stats for optimized roster
# -----------------------------------------------------------------------------

project_team_stats <- function(roster_df) {
  # Weight by usage rate for offense, equal-weighted for defense
  w_ortg  <- weighted.mean(roster_df$ortg, roster_df$usage)
  avg_drtg <- mean(roster_df$drtg)
  avg_orb  <- mean(roster_df$orb_pct)
  avg_3pct <- mean(roster_df$fg3_pct[roster_df$fg3_pct > 0])
  avg_bpm  <- mean(roster_df$bpm)

  cat("\n--- Projected Team Stats ---\n")
  cat(sprintf("  Adj. Offensive Rating : %.1f\n", w_ortg))
  cat(sprintf("  Adj. Defensive Rating : %.1f\n", avg_drtg))
  cat(sprintf("  Net Rating            : %+.1f\n", w_ortg - avg_drtg))
  cat(sprintf("  Avg. Off. Reb Rate    : %.1f%%\n", avg_orb * 100))
  cat(sprintf("  3P%% (shooters)       : %.1f%%\n", avg_3pct * 100))
  cat(sprintf("  Avg. BPM              : %+.1f\n", avg_bpm))

  # Rough national rank approximation (2026 KenPom landscape)
  # ~120 ortg = top-5 nationally; ~115 = top-25; ~112 = top-50
  kenpom_equiv <- round(w_ortg - avg_drtg, 1)
  rank_est <- dplyr::case_when(
    kenpom_equiv >= 22  ~ "Estimated KenPom Rank: Top 5",
    kenpom_equiv >= 18  ~ "Estimated KenPom Rank: 5-15",
    kenpom_equiv >= 14  ~ "Estimated KenPom Rank: 15-30",
    kenpom_equiv >= 10  ~ "Estimated KenPom Rank: 30-60",
    TRUE                ~ "Estimated KenPom Rank: 60+"
  )
  cat(sprintf("  Net Rating (adj.)     : %+.1f → %s\n\n", kenpom_equiv, rank_est))
}

# Run both scenarios
roster_a <- optimize_roster("A", roster_size = 7, print_results = TRUE)
project_team_stats(roster_a)

roster_b <- optimize_roster("B", roster_size = 7, print_results = TRUE)
project_team_stats(roster_b)
