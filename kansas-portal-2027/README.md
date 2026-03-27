# Kansas Basketball 2026–27 Portal Analysis

Transfer portal scenario planning and roster optimization for the 2026–27 season.

## Files

| File | Description |
|------|-------------|
| `player_data.R` | Player database — returning roster, freshmen, portal target pool with BartTorvik/EvanMiya-style metrics |
| `nil_market_model.R` | NIL salary inference model — estimates fair market value from efficiency stats + program exposure |
| `roster_optimizer.R` | Greedy roster optimizer — maximizes Team Efficiency Score within NIL budget + position constraints |
| `kansas_portal_report_2026.Rmd` | Full rendered report — Scenario A vs. B, visualizations, recommendations |

## Scenarios

- **Scenario A** — Flory Bidunga stays. ~$8M total roster budget, ~$2.3M for portal.
  Focus: premium guard(s) + wing depth.
- **Scenario B** — Bidunga leaves. ~$10.5M total, ~$7.6M for portal.
  Focus: center replacement + guard + wing.

## Running the Analysis

```r
# Install dependencies if needed
install.packages(c("dplyr", "tidyr", "ggplot2", "ggrepel",
                   "knitr", "kableExtra", "scales"))

# Run optimizer directly
source("roster_optimizer.R")

# Run NIL market model
source("nil_market_model.R")

# Render full report
rmarkdown::render("kansas_portal_report_2026.Rmd")
```

## Key Methodology Notes

- **Efficiency metrics** are based on BartTorvik / EvanMiya frameworks using
  2024–25 season data as the primary baseline with development trajectory
  adjustments for returning players.
- **NIL estimates** are inferred from a multiplier model anchored to publicly
  reported 2024–25 deals (On3, 247Sports). The model applies position scarcity,
  program exposure, and defensive premiums. Confidence interval: ±20–30%.
- **Portal availability** is a projection based on eligibility status and
  program context — not confirmed entries.
- All estimates should be re-calibrated when actual 2025–26 season stats and
  portal entry announcements become available.
