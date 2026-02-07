###############################################################################
# data_pipeline.R
# Load, clean, and prepare real college basketball data for parity analysis
# (Base R only)
#
# Data sources:
#   1. Kaggle CBB dataset (barttorvik metrics, 2013-2023)
#   2. NCAA_Hoops game results (for 2024 season extension)
#   3. Aggregate transfer portal volume (from NCAA/VerbalCommits)
#   4. Coaching changes (compiled from ESPN/HoopDirt)
###############################################################################

source("R/program_classification.R")

# ── 1. Load the Kaggle CBB dataset ───────────────────────────────────────────
load_kaggle_data <- function(filepath = "data/raw/cbb_kaggle.csv") {
  d <- read.csv(filepath, stringsAsFactors = FALSE)

  # Rename columns for consistency
  names(d) <- tolower(names(d))
  names(d)[names(d) == "adjoe"] <- "adj_off"
  names(d)[names(d) == "adjde"] <- "adj_def"

  # Compute derived variables
  d$win_pct <- d$w / d$g
  d$adj_margin <- d$adj_off - d$adj_def

  # Clean postseason: NA means no tournament
  d$postseason[d$postseason == "" | d$postseason == "N/A"] <- NA
  d$made_tourney <- as.integer(!is.na(d$postseason))

  # Numeric seed
  d$seed <- as.integer(d$seed)

  # Conference standardization
  d$conf <- trimws(d$conf)

  d
}

# ── 2. Compute 2024 season records from game results ─────────────────────────
compute_season_records <- function(results_file, year = 2024) {
  if (!file.exists(results_file)) return(NULL)

  games <- read.csv(results_file, stringsAsFactors = FALSE)

  # Don't filter on D1 flag — it's not reliably set; just exclude cancellations
  games <- games[!games$canceled & !games$postponed, ]

  games$win <- as.integer(games$teamscore > games$oppscore)

  records <- aggregate(win ~ team, data = games, FUN = sum)
  names(records)[2] <- "w"
  records$g <- aggregate(win ~ team, data = games, FUN = length)$win
  records$win_pct <- records$w / records$g
  records$year <- year

  records
}

# ── 3. Build the analysis panel ──────────────────────────────────────────────
build_analysis_panel <- function() {
  cat("Loading Kaggle CBB dataset...\n")
  kaggle <- load_kaggle_data()
  cat(sprintf("  Loaded %d team-seasons (%d-%d)\n",
              nrow(kaggle), min(kaggle$year), max(kaggle$year)))

  # Try to add 2024 data from game results
  results_file <- "/tmp/ncaa_2024_results.csv"
  if (file.exists(results_file)) {
    cat("Computing 2024 season records from game results...\n")
    records_2024 <- compute_season_records(results_file, year = 2024)
    if (!is.null(records_2024)) {
      # Match team names (NCAA_Hoops uses slightly different names)
      # Merge only the columns available from game results
      records_2024$conf <- NA  # Will try to fill from previous year
      records_2024$adj_off <- NA
      records_2024$adj_def <- NA
      records_2024$barthag <- NA
      records_2024$adj_margin <- NA
      records_2024$postseason <- NA
      records_2024$made_tourney <- NA
      records_2024$seed <- NA

      # Fill conference from most recent year
      last_conf <- kaggle[kaggle$year == max(kaggle$year),
                           c("team", "conf")]
      records_2024 <- merge(records_2024, last_conf, by = "team", all.x = TRUE)
      records_2024$conf <- records_2024$conf.y
      records_2024$conf.x <- NULL
      records_2024$conf.y <- NULL

      # Add columns that exist in kaggle but not in records
      for (col in setdiff(names(kaggle), names(records_2024))) {
        records_2024[[col]] <- NA
      }

      # Bind
      kaggle <- rbind(kaggle[, names(kaggle)],
                       records_2024[, names(kaggle)])
      cat(sprintf("  Added %d teams for 2024 season\n", nrow(records_2024)))
    }
  }

  # ── Classify programs ──
  kaggle <- classify_teams(kaggle, name_col = "team", conf_col = "conf")

  # ── Define policy periods ──
  kaggle$pre_portal  <- as.integer(kaggle$year <= 2019)
  kaggle$post_portal <- as.integer(kaggle$year >= 2021)
  kaggle$transition  <- as.integer(kaggle$year == 2020)

  # ── Add aggregate transfer portal volume data ──
  # Source: NCAA.org + VerbalCommits via Front Office Sports
  portal_volume <- data.frame(
    year = c(2019, 2020, 2021, 2022, 2023, 2024, 2025),
    portal_entries = c(957, 967, 1653, 1650, 1724, 2083, 2320)
  )
  kaggle <- merge(kaggle, portal_volume, by = "year", all.x = TRUE)
  kaggle$portal_entries[is.na(kaggle$portal_entries)] <- 0

  # ── Coaching change counts by year (approximate from public records) ──
  # These are D1 coaching changes per year (firings + resignations)
  coaching_changes <- data.frame(
    year = 2013:2024,
    coaching_changes = c(38, 42, 35, 40, 45, 43, 40, 32, 38, 44, 48, 52)
  )
  kaggle <- merge(kaggle, coaching_changes, by = "year", all.x = TRUE)

  # Sort
  kaggle <- kaggle[order(kaggle$team, kaggle$year), ]
  rownames(kaggle) <- NULL

  cat(sprintf("  Final panel: %d team-seasons, %d unique teams, years %d-%d\n",
              nrow(kaggle), length(unique(kaggle$team)),
              min(kaggle$year), max(kaggle$year)))

  kaggle
}

# ── 4. Summary statistics ────────────────────────────────────────────────────
print_data_summary <- function(panel) {
  cat("\n========================================\n")
  cat("  DATA SUMMARY\n")
  cat("========================================\n\n")

  cat("Observations by year:\n")
  print(table(panel$year))

  cat("\nObservations by program tier:\n")
  print(table(panel$program_tier))

  cat("\nObservations by period:\n")
  cat(sprintf("  Pre-portal (<=2019):  %d\n", sum(panel$pre_portal)))
  cat(sprintf("  Post-portal (>=2021): %d\n", sum(panel$post_portal)))
  cat(sprintf("  Transition (2020):    %d\n", sum(panel$transition)))

  cat("\nKey variables:\n")
  desc_vars <- c("w", "g", "win_pct")
  if ("adj_margin" %in% names(panel)) {
    desc_vars <- c(desc_vars, "adj_margin", "adj_off", "adj_def")
  }

  for (v in desc_vars) {
    if (v %in% names(panel)) {
      x <- panel[[v]]
      x <- x[!is.na(x)]
      cat(sprintf("  %-15s mean=%.2f  sd=%.2f  [%.2f, %.2f]\n",
                  v, mean(x), sd(x), min(x), max(x)))
    }
  }

  cat("\nBlue Bloods in dataset:\n")
  bb <- unique(panel$team[panel$program_tier == "Blue Blood"])
  cat(sprintf("  %s\n", paste(bb, collapse = ", ")))

  cat("\nNear-Blue Bloods in dataset:\n")
  nbb <- unique(panel$team[panel$program_tier == "Near-Blue Blood"])
  cat(sprintf("  %s\n", paste(nbb, collapse = ", ")))
}
