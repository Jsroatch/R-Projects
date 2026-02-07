###############################################################################
# simulate_data.R
# Generate realistic college basketball transfer portal data for causal analysis
# (Base R only — no external dependencies)
#
# The simulation encodes key features of the actual transfer market:
#   - Teams have persistent quality (latent program strength)
#   - Players have measurable talent (efficiency metrics)
#   - Transfer decisions depend on program fit, geography, and idiosyncratic factors
#   - Coaching changes at origin schools create exogenous portal entry shocks
###############################################################################

simulate_transfer_data <- function(
    n_teams     = 120,
    n_seasons   = 6,       # e.g., 2019-2024
    seed        = 42
) {
  set.seed(seed)

  positions  <- c("PG", "SG", "SF", "PF", "C")
  conferences <- c("ACC", "Big12", "BigTen", "BigEast", "SEC", "Pac12",
                    "AAC", "MWC", "WCC", "A10", "MVC", "CUSA")
  power_conf  <- c("ACC", "Big12", "BigTen", "BigEast", "SEC", "Pac12")
  conf_probs  <- c(.12, .10, .10, .08, .12, .08, .08, .07, .05, .06, .07, .07)

  # Helper: Gumbel random variates (for logit-consistent choice shocks)
  rgumbel <- function(n, loc = 0, scale = 1) {
    loc - scale * log(-log(runif(n)))
  }

  # ── 1. Team-level data ─────────────────────────────────────────────────────
  teams <- data.frame(
    team_id       = 1:n_teams,
    team_name     = paste0("Team_", sprintf("%03d", 1:n_teams)),
    conference    = sample(conferences, n_teams, replace = TRUE, prob = conf_probs),
    geo_x         = rnorm(n_teams),
    geo_y         = rnorm(n_teams),
    base_strength = rnorm(n_teams),
    stringsAsFactors = FALSE
  )
  teams$power_conf <- as.integer(teams$conference %in% power_conf)

  # ── 2. Team-season panel with AR(1) evolving strength ──────────────────────
  team_seasons <- expand.grid(team_id = 1:n_teams, season = 1:n_seasons)
  team_seasons <- merge(team_seasons, teams, by = "team_id")
  team_seasons <- team_seasons[order(team_seasons$team_id, team_seasons$season), ]

  # AR(1) program strength
  team_seasons$program_strength <- NA_real_
  for (tid in 1:n_teams) {
    idx <- which(team_seasons$team_id == tid)
    strength <- team_seasons$base_strength[idx[1]]
    for (j in seq_along(idx)) {
      shock <- rnorm(1, 0, 0.3)
      strength <- 0.85 * strength + shock
      team_seasons$program_strength[idx[j]] <- strength
    }
  }

  # ── 3. Coaching changes (exogenous shock to portal supply) ─────────────────
  team_seasons$coaching_change <- rbinom(nrow(team_seasons), 1, 0.08)

  # ── 4. Position needs (pre-determined) ─────────────────────────────────────
  pos_needs <- expand.grid(
    team_id  = 1:n_teams,
    season   = 1:n_seasons,
    position = positions,
    stringsAsFactors = FALSE
  )
  pos_needs$position_need <- pmax(0, rnorm(nrow(pos_needs), 0.5, 0.4))

  # ── 5. Generate transfer portal entrants ───────────────────────────────────
  all_transfers <- data.frame()

  for (s in 1:n_seasons) {
    base_rate <- 1.5 + 0.3 * s
    ts <- team_seasons[team_seasons$season == s, ]

    for (i in seq_len(nrow(ts))) {
      rate <- base_rate * (1 + 2.5 * ts$coaching_change[i])
      n_trans <- rpois(1, rate)
      if (n_trans == 0) next

      players <- data.frame(
        player_id = ((s - 1) * 10000) + seq_len(n_trans) + (i - 1) * 100,
        season    = s,
        origin_team_id = ts$team_id[i],
        position  = sample(positions, n_trans, replace = TRUE),
        player_talent = 0.3 * ts$program_strength[i] + rnorm(n_trans),
        coaching_change_transfer = rbinom(n_trans, 1,
          ifelse(ts$coaching_change[i] == 1, 0.7, 0.1)),
        player_geo_x = ts$geo_x[i] + rnorm(n_trans, 0, 0.5),
        player_geo_y = ts$geo_y[i] + rnorm(n_trans, 0, 0.5),
        stringsAsFactors = FALSE
      )
      all_transfers <- rbind(all_transfers, players)
    }
  }

  # ── 6. Assign transfer destinations (multinomial logit choice) ─────────────
  # Players choose based on program strength, position need, geography,
  # conference prestige, and a large idiosyncratic Gumbel shock.
  all_transfers$dest_team_id <- NA_integer_

  for (s in 1:n_seasons) {
    idx_s <- which(all_transfers$season == s)
    if (length(idx_s) == 0) next

    ts <- team_seasons[team_seasons$season == s, ]
    pn <- pos_needs[pos_needs$season == s, ]

    for (k in idx_s) {
      origin <- all_transfers$origin_team_id[k]
      pos    <- all_transfers$position[k]
      pgx    <- all_transfers$player_geo_x[k]
      pgy    <- all_transfers$player_geo_y[k]

      eligible <- ts[ts$team_id != origin, ]
      needs    <- pn[pn$position == pos, c("team_id", "position_need")]
      eligible <- merge(eligible, needs, by = "team_id", all.x = TRUE)
      eligible$position_need[is.na(eligible$position_need)] <- 0.3

      # Utility components
      prog_util   <- 0.8 * eligible$program_strength
      need_util   <- 0.5 * eligible$position_need
      geo_dist    <- -0.4 * sqrt((eligible$geo_x - pgx)^2 +
                                  (eligible$geo_y - pgy)^2)
      power_bonus <- 0.3 * eligible$power_conf
      idio_shock  <- rgumbel(nrow(eligible))

      total_util <- prog_util + need_util + geo_dist + power_bonus + idio_shock
      all_transfers$dest_team_id[k] <- eligible$team_id[which.max(total_util)]
    }

    if (s %% 2 == 0) cat(sprintf("   Season %d/%d assigned\n", s, n_seasons))
  }

  # ── 7. Aggregate transfer talent flows ─────────────────────────────────────
  # Talent received
  talent_in <- aggregate(
    cbind(n_transfers_in = player_talent,
          total_talent_in = player_talent,
          max_talent_in = player_talent,
          n_coaching_transfers = coaching_change_transfer) ~ dest_team_id + season,
    data = all_transfers,
    FUN = function(x) x[1]  # placeholder
  )
  # Re-aggregate properly
  talent_in <- do.call(rbind, lapply(
    split(all_transfers, list(all_transfers$dest_team_id, all_transfers$season)),
    function(d) {
      if (nrow(d) == 0) return(NULL)
      data.frame(
        dest_team_id        = d$dest_team_id[1],
        season              = d$season[1],
        n_transfers_in      = nrow(d),
        total_talent_in     = sum(d$player_talent),
        mean_talent_in      = mean(d$player_talent),
        max_talent_in       = max(d$player_talent),
        n_coaching_transfers = sum(d$coaching_change_transfer)
      )
    }
  ))

  # Talent lost
  talent_out <- do.call(rbind, lapply(
    split(all_transfers, list(all_transfers$origin_team_id, all_transfers$season)),
    function(d) {
      if (nrow(d) == 0) return(NULL)
      data.frame(
        origin_team_id   = d$origin_team_id[1],
        season           = d$season[1],
        n_transfers_out  = nrow(d),
        total_talent_out = sum(d$player_talent)
      )
    }
  ))

  # ── 8. Team outcomes ───────────────────────────────────────────────────────
  TRUE_BETA <- 2.5

  panel <- team_seasons
  panel <- merge(panel, talent_in,
                 by.x = c("team_id", "season"),
                 by.y = c("dest_team_id", "season"),
                 all.x = TRUE)
  panel <- merge(panel, talent_out,
                 by.x = c("team_id", "season"),
                 by.y = c("origin_team_id", "season"),
                 all.x = TRUE)

  # Fill NAs with 0
  fill_cols <- c("n_transfers_in", "total_talent_in", "mean_talent_in",
                 "max_talent_in", "n_coaching_transfers",
                 "n_transfers_out", "total_talent_out")
  for (col in fill_cols) {
    panel[[col]][is.na(panel[[col]])] <- 0
  }

  panel$net_talent <- panel$total_talent_in - panel$total_talent_out

  # Generate outcomes
  panel$latent_wins <- 15 + 3 * panel$program_strength +
                       TRUE_BETA * panel$net_talent + rnorm(nrow(panel), 0, 2)
  panel$wins <- pmin(pmax(round(panel$latent_wins), 3), 33)
  panel$adj_efficiency <- 4 * panel$program_strength +
                          3.0 * panel$net_talent + rnorm(nrow(panel), 0, 3)
  panel$made_tournament <- as.integer(panel$latent_wins > 20)

  # ── 9. Build shift-share (Bartik) instrument ───────────────────────────────
  # Shares: team's historical recruiting pattern by position × origin conference
  transfers_with_conf <- merge(
    all_transfers,
    teams[, c("team_id", "conference")],
    by.x = "origin_team_id", by.y = "team_id"
  )
  names(transfers_with_conf)[names(transfers_with_conf) == "conference"] <-
    "origin_conference"

  # Compute shares
  shares <- aggregate(
    player_id ~ dest_team_id + position + origin_conference,
    data = transfers_with_conf,
    FUN = length
  )
  names(shares)[4] <- "hist_count"

  # Normalize to shares within each destination team
  team_totals <- aggregate(hist_count ~ dest_team_id, data = shares, FUN = sum)
  names(team_totals)[2] <- "team_total"
  shares <- merge(shares, team_totals, by = "dest_team_id")
  shares$share <- shares$hist_count / shares$team_total

  # Shifts: coaching-change driven supply shocks by position × origin conference
  cc_transfers <- transfers_with_conf[transfers_with_conf$coaching_change_transfer == 1, ]
  if (nrow(cc_transfers) > 0) {
    shifts <- do.call(rbind, lapply(
      split(cc_transfers,
            list(cc_transfers$season, cc_transfers$position,
                 cc_transfers$origin_conference)),
      function(d) {
        if (nrow(d) == 0) return(NULL)
        data.frame(
          season              = d$season[1],
          position            = d$position[1],
          origin_conference   = d$origin_conference[1],
          supply_shock_talent = sum(d$player_talent),
          supply_shock_count  = nrow(d)
        )
      }
    ))
  } else {
    shifts <- data.frame(
      season = integer(), position = character(),
      origin_conference = character(),
      supply_shock_talent = numeric(), supply_shock_count = integer()
    )
  }

  # Bartik instrument: sum over cells of share × shift
  bartik_raw <- merge(shares, shifts, by = c("position", "origin_conference"))
  if (nrow(bartik_raw) > 0) {
    bartik_raw$bartik_talent_cell <- bartik_raw$share * bartik_raw$supply_shock_talent
    bartik_raw$bartik_count_cell  <- bartik_raw$share * bartik_raw$supply_shock_count

    bartik <- aggregate(
      cbind(bartik_talent = bartik_talent_cell,
            bartik_count = bartik_count_cell) ~ dest_team_id + season,
      data = bartik_raw,
      FUN = sum
    )
  } else {
    bartik <- data.frame(
      dest_team_id = integer(), season = integer(),
      bartik_talent = numeric(), bartik_count = numeric()
    )
  }

  panel <- merge(panel, bartik,
                 by.x = c("team_id", "season"),
                 by.y = c("dest_team_id", "season"),
                 all.x = TRUE)
  panel$bartik_talent[is.na(panel$bartik_talent)] <- 0
  panel$bartik_count[is.na(panel$bartik_count)]   <- 0

  # Sort
  panel <- panel[order(panel$team_id, panel$season), ]
  rownames(panel) <- NULL

  # ── Return ─────────────────────────────────────────────────────────────────
  list(
    teams      = teams,
    team_panel = panel,
    transfers  = all_transfers,
    shares     = shares,
    shifts     = shifts,
    bartik     = bartik,
    true_beta  = TRUE_BETA,
    params     = list(n_teams = n_teams, n_seasons = n_seasons, seed = seed)
  )
}
