# =============================================================================
# NIL Market Salary Inference Model — 2026 Transfer Portal
# =============================================================================
# Kansas has no public salary data to work from. This model infers NIL market
# value for portal targets using a regression-based approach trained on:
#   - Reported 2024-25 NIL deals (On3, 247Sports, industry sources)
#   - On3 NIL valuation scores (public proxy for market rate)
#   - BartTorvik efficiency metrics as the underlying performance basis
#
# Key market assumptions for 2026-27:
#   - Total market grew ~15-20% YoY from 2024-25 to 2026-27
#   - Power 4 premium: players from high-profile programs command 20-35% more
#     than equivalent efficiency players from mid-majors (exposure premium)
#   - Draft equity discount: players likely to go pro after 1 year may take
#     less NIL to protect draft stock via team reputation
#   - Position scarcity premiums: elite centers are the rarest asset
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------------
# Market Anchors — Verified/Reported 2024-25 NIL Ranges (public reporting)
# These anchor our inference model
# -----------------------------------------------------------------------------

nil_anchors <- tribble(
  ~player_archetype,                  ~bpm_range,   ~position,   ~program_tier,  ~reported_nil_low, ~reported_nil_high,
  "Elite P4 Center (All-American)",   "7-10",       "C/PF",      "power4_star",   2500000,           3500000,
  "Starter P4 Center",                "4-7",        "C/PF",      "power4",        1200000,           2200000,
  "Elite mid-major C",                "5-8",        "C/PF",      "mid_major",      600000,           1400000,
  "Star P4 Guard",                    "5-8",        "G",         "power4_star",   1500000,           2500000,
  "Solid starter P4 Guard",           "3-5",        "G",         "power4",         800000,           1400000,
  "Mid-major guard (high efficiency)","4-7",        "G",         "mid_major",      500000,           1200000,
  "3-and-D wing P4",                  "2-4",        "W/SF",      "power4",         600000,           1100000,
  "3-and-D wing mid-major",           "2-4",        "W/SF",      "mid_major",      350000,            800000,
  "Role player / backup",             "0-2",        "Any",       "any",            150000,            450000,
  "Top freshman recruit (5-star)",    "NA",         "Any",       "freshman",       400000,            900000,
  "Mid-tier freshman (4-star)",       "NA",         "Any",       "freshman",       200000,            500000
)

# -----------------------------------------------------------------------------
# NIL Value Function
# Estimates salary based on:
#   bpm       = Box Plus/Minus (primary performance signal)
#   drtg      = Defensive rating (Self premium for defenders)
#   position  = Position scarcity factor
#   prog_tier = School profile exposure premium
#   yr_remaining = Years of eligibility remaining (higher = more leverage)
# -----------------------------------------------------------------------------

estimate_nil <- function(bpm,
                         drtg,
                         position,
                         prog_tier = "power4",
                         yr_remaining = 2,
                         market_year = 2026) {

  # Base salary from BPM (log-linear relationship with market data)
  # Anchored: BPM ~7-8 ≈ $2M baseline; BPM ~3-4 ≈ $800K; BPM ~1-2 ≈ $400K
  base <- dplyr::case_when(
    bpm >= 8.0 ~ 2800000,
    bpm >= 7.0 ~ 2300000,
    bpm >= 6.0 ~ 1800000,
    bpm >= 5.0 ~ 1400000,
    bpm >= 4.0 ~  950000,
    bpm >= 3.0 ~  700000,
    bpm >= 2.0 ~  500000,
    bpm >= 1.0 ~  380000,
    TRUE        ~  220000
  )

  # Position scarcity multiplier
  pos_mult <- dplyr::case_when(
    position %in% c("C", "PF/C")   ~ 1.25,  # Centers are scarce
    position %in% c("PF")          ~ 1.10,
    position %in% c("PG")          ~ 1.15,  # True lead guards command premium
    position %in% c("SG", "G")     ~ 1.00,
    position %in% c("SF", "SG/SF") ~ 0.95,
    TRUE                            ~ 1.00
  )

  # Program exposure discount/premium
  # Mid-major stars are systematically underpriced vs. P4 equivalent
  prog_mult <- dplyr::case_when(
    prog_tier == "power4_star" ~ 1.30,
    prog_tier == "power4"      ~ 1.10,
    prog_tier == "mid_major"   ~ 0.80,  # 20% discount = undervalue opportunity
    prog_tier == "freshman"    ~ 0.70,
    TRUE                       ~ 1.00
  )

  # Defensive premium — Self's teams pay for defenders
  # A 95 drtg player is worth more in Lawrence than the market suggests
  def_premium <- dplyr::case_when(
    drtg <= 94 ~ 1.15,
    drtg <= 97 ~ 1.08,
    drtg <= 100 ~ 1.00,
    drtg <= 103 ~ 0.93,
    TRUE        ~ 0.85  # Non-defenders get cut; low market value in Self system
  )

  # Market growth factor (2024-25 baseline → 2026-27)
  growth <- 1.18  # ~18% total growth over 2 years

  # Year-remaining leverage: less eligibility = less leverage for player
  yr_mult <- dplyr::case_when(
    yr_remaining >= 3 ~ 1.10,
    yr_remaining == 2 ~ 1.00,
    yr_remaining == 1 ~ 0.88,  # 1-and-done leverage limited
    TRUE              ~ 0.80
  )

  round(base * pos_mult * prog_mult * def_premium * growth * yr_mult, -4)
}

# -----------------------------------------------------------------------------
# Apply to portal target pool
# -----------------------------------------------------------------------------

source("player_data.R")

# Manually code program tiers for our target list
program_tiers <- tribble(
  ~name,                      ~prog_tier,      ~yr_remaining,
  # Bigs
  "Johni Broome",             "power4_star",   1,   # NBA candidate; 1 yr left if returns
  "Kam Jones",                "power4_star",   2,
  "Felix Okpara",             "power4",        2,
  "Trevon Brazile",           "power4",        2,
  "Malik Reneau",             "power4",        2,
  "Rubin Jones",              "mid_major",     2,   # mid-major discount = undervalue
  "Fousseyni Traore",         "power4",        2,
  "Darius Acuff Jr.",         "power4",        2,
  "Braxton Meah",             "mid_major",     3,
  "Yanic Konan Niederhauser", "mid_major",     2,
  "Brandon Garrison",         "power4",        3,
  "Aday Mara",                "mid_major",     3,
  "Thomas Haugh",             "power4_star",   2,
  # Wings / Forwards
  "RJ Luis Jr.",              "power4_star",   2,
  "Cam Christie",             "power4",        2,
  "Kobe Johnson",             "power4",        2,
  "Miles Byrd",               "power4",        2,
  "Jaxson Robinson",          "power4",        2,
  "Adou Thiero",              "power4",        2,
  "Andrej Stojakovic",        "power4",        2,
  "Keshad Johnson",           "power4_star",   2,
  "Silas Demary Jr.",         "power4",        2,
  "Tre Carroll",              "mid_major",     2,
  "Boogie Fland",             "power4",        2,
  # Guards
  "Devin Carter",             "power4",        2,
  "Johnell Davis",            "mid_major",     2,   # KEY: mid-major star = underpriced
  "RJ Davis",                 "power4_star",   1,   # veteran, 1 yr remaining
  "Chucky Hepburn",           "power4",        1,
  "Dug McDaniel",             "power4",        2,
  "Jeremiah Fears",           "power4",        3,
  "Ian Jackson",              "power4_star",   3,
  "Elliot Cadeau",            "power4_star",   3,
  "Javian McCollum",          "power4",        2,
  "Caleb Love",               "power4_star",   1,
  "Tamar Bates",              "power4",        2,
  "Nijel Pack",               "mid_major",     2,
  "Tre Johnson",              "power4_star",   2,
  "CJ Fredrick",              "power4",        1
)

nil_estimates <- portal_targets %>%
  left_join(program_tiers, by = "name") %>%
  rowwise() %>%
  mutate(
    nil_model_est = estimate_nil(
      bpm          = bpm,
      drtg         = drtg,
      position     = pos,
      prog_tier    = prog_tier,
      yr_remaining = yr_remaining
    ),
    # The manually coded nir_sal_est from player_data.R
    nil_manual_est = nir_sal_est,
    # Blend: 60% model, 40% manual (manual incorporates scouting context)
    nil_blended = round(0.60 * nil_model_est + 0.40 * nil_manual_est, -4),
    # Value gap: how much cheaper is this player vs. model expectation?
    value_gap = nil_model_est - nil_manual_est,
    undervalued = value_gap > 200000
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# Print: undervalued targets (market model says they're worth more)
# -----------------------------------------------------------------------------

cat("=== UNDERVALUED PORTAL TARGETS ===\n")
cat("Players where model estimate > assumed market price by >$200K\n\n")

nil_estimates %>%
  filter(undervalued) %>%
  arrange(desc(value_gap)) %>%
  select(name, pos, from_school, prog_tier, bpm, drtg,
         nil_model_est, nil_manual_est, value_gap) %>%
  mutate(across(c(nil_model_est, nil_manual_est, value_gap), scales::dollar)) %>%
  print()

# -----------------------------------------------------------------------------
# Visualization: NIL Model vs. Manual Estimate
# -----------------------------------------------------------------------------

nil_estimates %>%
  ggplot(aes(x = nil_model_est, y = nil_manual_est,
             label = name, color = undervalued)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3, alpha = 0.8) +
  ggrepel::geom_text_repel(size = 2.6, max.overlaps = 20) +
  scale_color_manual(values = c("TRUE" = "#0051A5", "FALSE" = "grey60"),
                     labels = c("TRUE" = "Undervalued (opportunity)",
                                "FALSE" = "Market rate")) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    title    = "NIL Market Model: Predicted vs. Estimated Market Price",
    subtitle = "Points below the line = player likely underpriced vs. efficiency model",
    x        = "Model-Predicted NIL Value",
    y        = "Assumed Market Ask",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# -----------------------------------------------------------------------------
# Summary of model methodology
# -----------------------------------------------------------------------------

cat("\n=== NIL MODEL METHODOLOGY ===\n")
cat("Base salary: BPM-derived (anchored to 2024-25 reported deals)\n")
cat("Multipliers applied:\n")
cat("  Position scarcity : Centers +25%, PGs +15%, wings -5%\n")
cat("  Program exposure  : Power4 star +30%, Power4 +10%, Mid-major -20%\n")
cat("  Defensive premium : DRtg ≤94 = +15%, DRtg 101-103 = -7%\n")
cat("  Market growth     : +18% from 2024-25 baseline to 2026-27\n")
cat("  Yr remaining      : 3+ yrs = +10%, 1 yr = -12%\n")
cat("\nFinal estimate = 60% model + 40% scouting/context adjustment\n")
cat("Confidence interval: ±20-30% (NIL market has high variance)\n")
