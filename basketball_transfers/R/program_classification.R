###############################################################################
# program_classification.R
# Classify D1 men's basketball programs into tiers for heterogeneous
# treatment analysis of transfer portal / NIL policy effects
#
# Tiers:
#   1. Blue Bloods — historically dominant programs (top ~8)
#   2. Elite / Near-Blue-Bloods — perennial tournament teams, high resources
#   3. High-Major — power conference programs with strong but variable history
#   4. Mid-Major — non-power conference programs
#
# Classification is based on pre-portal-era (2010-2019) performance to
# avoid post-treatment contamination.
###############################################################################

# Blue Bloods: Programs with sustained historical dominance
# Criteria: multiple national championships, consistent top-10 finishes,
# brand recognition, recruiting dominance
BLUE_BLOODS <- c(
  "Duke",
  "Kansas",
  "Kentucky",
  "North Carolina",
  "UCLA",
  "Indiana"
)

# Near-Blue-Bloods / Elite programs: Perennial contenders, large budgets,
# but not quite the sustained multi-decade dominance of blue bloods
NEAR_BLUE_BLOODS <- c(
  "Villanova",
  "Michigan St.",
  "Louisville",
  "Syracuse",
  "Connecticut",
  "Arizona",
  "Gonzaga",
  "Florida",
  "Ohio St.",
  "Michigan",
  "Wisconsin",
  "Virginia"
)

# Power conferences (as of the study period)
POWER_CONFERENCES <- c("ACC", "B10", "B12", "BE", "SEC", "P12")

# Alternative names mapping (barttorvik vs common usage)
TEAM_NAME_MAP <- list(
  "North Carolina" = c("UNC", "N. Carolina", "North Carolina"),
  "Michigan St."   = c("Michigan State", "Michigan St.", "MSU"),
  "Ohio St."       = c("Ohio State", "Ohio St.", "OSU"),
  "Connecticut"    = c("UConn", "Connecticut", "UCONN")
)

# Classify a team given its conference and name
classify_program <- function(team_name, conference = NA) {
  if (team_name %in% BLUE_BLOODS) return("Blue Blood")
  if (team_name %in% NEAR_BLUE_BLOODS) return("Near-Blue Blood")
  if (!is.na(conference) && conference %in% POWER_CONFERENCES) return("High-Major")
  return("Mid-Major")
}

# Classify an entire data frame of teams
classify_teams <- function(teams_df, name_col = "team", conf_col = "conf") {
  teams_df$program_tier <- mapply(
    classify_program,
    teams_df[[name_col]],
    if (conf_col %in% names(teams_df)) teams_df[[conf_col]] else NA
  )
  teams_df$program_tier <- factor(
    teams_df$program_tier,
    levels = c("Blue Blood", "Near-Blue Blood", "High-Major", "Mid-Major")
  )
  teams_df
}

###############################################################################
# Parity metrics
###############################################################################

# Gini coefficient of win percentages
gini_coefficient <- function(x) {
  x <- sort(x)
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA)
  numerator <- 2 * sum(seq_along(x) * x) - (n + 1) * sum(x)
  denominator <- n * sum(x)
  numerator / denominator
}

# Herfindahl-Hirschman Index of win shares
hhi_wins <- function(wins) {
  total <- sum(wins)
  if (total == 0) return(NA)
  shares <- wins / total
  sum(shares^2)
}

# Noll-Scully ratio: ratio of actual SD of win% to idealized SD
# Idealized SD under perfect parity = 0.5 / sqrt(n_games)
noll_scully <- function(win_pct, n_games = 30) {
  actual_sd <- sd(win_pct)
  ideal_sd  <- 0.5 / sqrt(n_games)
  actual_sd / ideal_sd
}

# Top-N concentration ratio: share of total wins held by top N teams
top_n_concentration <- function(wins, n = 10) {
  wins <- sort(wins, decreasing = TRUE)
  sum(head(wins, n)) / sum(wins)
}

# Tournament bid dispersion: how many unique teams make the tournament
# over a window of years (higher = more parity)
tournament_uniqueness <- function(tournament_teams_by_year) {
  # tournament_teams_by_year: list of character vectors
  all_teams <- unlist(tournament_teams_by_year)
  n_total   <- length(all_teams)
  n_unique  <- length(unique(all_teams))
  n_unique / n_total  # ratio: 1 = all different, lower = repeat participants
}

# Compute all parity metrics for a single season
compute_parity_metrics <- function(season_data, wins_col = "wins",
                                    games_col = "games") {
  wins <- season_data[[wins_col]]
  games <- season_data[[games_col]]
  win_pct <- wins / games

  data.frame(
    gini           = gini_coefficient(win_pct),
    hhi            = hhi_wins(wins),
    noll_scully    = noll_scully(win_pct, median(games)),
    top10_share    = top_n_concentration(wins, 10),
    top25_share    = top_n_concentration(wins, 25),
    sd_win_pct     = sd(win_pct),
    range_win_pct  = diff(range(win_pct)),
    median_wins    = median(wins),
    n_teams        = length(wins)
  )
}

# Compute parity metrics by season across a panel
compute_parity_panel <- function(panel, season_col = "season",
                                  wins_col = "wins", games_col = "games") {
  seasons <- sort(unique(panel[[season_col]]))

  parity_panel <- do.call(rbind, lapply(seasons, function(s) {
    sdata <- panel[panel[[season_col]] == s, ]
    metrics <- compute_parity_metrics(sdata, wins_col, games_col)
    metrics$season <- s
    metrics
  }))

  parity_panel
}

# Compute within-tier parity (e.g., SD of wins among blue bloods only)
compute_tier_parity <- function(panel, season_col = "season",
                                 wins_col = "wins", tier_col = "program_tier") {
  tiers   <- levels(panel[[tier_col]])
  seasons <- sort(unique(panel[[season_col]]))

  do.call(rbind, lapply(tiers, function(tier) {
    do.call(rbind, lapply(seasons, function(s) {
      d <- panel[panel[[season_col]] == s & panel[[tier_col]] == tier, ]
      if (nrow(d) < 3) return(NULL)
      data.frame(
        tier    = tier,
        season  = s,
        n_teams = nrow(d),
        mean_wins   = mean(d[[wins_col]]),
        sd_wins     = sd(d[[wins_col]]),
        median_wins = median(d[[wins_col]])
      )
    }))
  }))
}

# Between-tier disparity: gap in mean wins between blue bloods and others
compute_between_tier_gap <- function(panel, season_col = "season",
                                      wins_col = "wins",
                                      tier_col = "program_tier") {
  seasons <- sort(unique(panel[[season_col]]))

  do.call(rbind, lapply(seasons, function(s) {
    d <- panel[panel[[season_col]] == s, ]
    tier_means <- tapply(d[[wins_col]], d[[tier_col]], mean, na.rm = TRUE)

    bb_mean <- tier_means["Blue Blood"]
    hm_mean <- tier_means["High-Major"]
    mm_mean <- tier_means["Mid-Major"]

    data.frame(
      season = s,
      bb_mean = as.numeric(bb_mean),
      hm_mean = as.numeric(hm_mean),
      mm_mean = as.numeric(mm_mean),
      bb_hm_gap = as.numeric(bb_mean - hm_mean),
      bb_mm_gap = as.numeric(bb_mean - mm_mean)
    )
  }))
}
