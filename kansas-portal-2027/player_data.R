# =============================================================================
# Kansas Basketball 2026-27 Portal Analysis — Player Database
# Data sourced from BartTorvik / EvanMiya season-long efficiency profiles
# NOTE: 2025-26 stats are projections/estimates based on prior-season trends
#       and scouting context. Portal availability reflects anticipated entries.
# =============================================================================

library(dplyr)
library(tibble)

# -----------------------------------------------------------------------------
# RETURNING KANSAS PLAYERS
# -----------------------------------------------------------------------------
# BartTorvik key stats:
#   ortg  = offensive rating (points per 100 possessions)
#   drtg  = defensive rating (points allowed per 100 while on floor)
#   bpm   = box plus/minus per 100 possessions
#   obpm  = offensive BPM
#   dbpm  = defensive BPM
#   usage = usage rate (% of team possessions used)
#   min_pct = approximate % of minutes played
#   orb_pct = offensive rebound rate
#   fg3_pct = 3-point percentage
#   fg3_rate= 3-point attempt rate (3PA / FGA)
#   ts_pct  = true shooting %
#   stl_pct = steal rate
#   blk_pct = block rate
#   nir_sal = estimated NIL salary (USD) for 2026-27 negotiation

kansas_returning <- tribble(
  ~name,              ~pos,  ~ht,    ~yr_26_27, ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal,
  "Flory Bidunga",    "C",   "6-10", "Jr",       118,   96,    6.8,   3.2,   3.6,   0.22,   0.128,    0.000,    0.00,      0.620,   0.012,    0.110,    2800000,
  "Marko Pielic",     "W",   "6-7",  "Jr",       115,   101,   3.9,   2.8,   1.1,   0.18,   0.042,    0.378,    0.48,      0.590,   0.014,    0.020,     900000,
  "JMari Williams",   "G",   "6-3",  "Jr",       112,   99,    2.8,   1.9,   0.9,   0.20,   0.028,    0.348,    0.38,      0.565,   0.022,    0.010,     750000
)

# -----------------------------------------------------------------------------
# INCOMING FRESHMEN — Kansas 2026-27 Class
# -----------------------------------------------------------------------------
# TK = confirmed high-upside recruit; others are "question mark" freshmen.
# Freshman stats are baseline projections — BartTorvik historically discounts
# freshmen ~8-12 pts ortg relative to upperclassman equivalents.

kansas_freshmen <- tribble(
  ~name,              ~pos,  ~ht,    ~yr_26_27, ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal, ~certainty,
  "TK (Fr.)",         "PF",  "6-8",  "Fr",       108,   102,   1.8,   1.2,   0.6,   0.18,   0.085,    0.310,    0.28,      0.540,   0.015,    0.055,     600000,   "confirmed",
  "Fr. Recruit #2",   "G",   "6-4",  "Fr",       104,   103,   0.6,   0.4,   0.2,   0.16,   0.025,    0.330,    0.40,      0.525,   0.018,    0.008,     350000,   "question_mark",
  "Fr. Recruit #3",   "W",   "6-6",  "Fr",       103,   104,   0.2,   0.1,   0.1,   0.14,   0.040,    0.300,    0.35,      0.510,   0.012,    0.012,     300000,   "question_mark"
)

# -----------------------------------------------------------------------------
# TRANSFER PORTAL TARGETS — 2026 Pool
# -----------------------------------------------------------------------------
# Annotated with "value_tier":
#   "premium"    = known star, market-rate or above
#   "value"      = strong metrics, likely underpriced due to school profile/fit
#   "deep_value" = high efficiency in limited role; could emerge in right system
#
# "self_fit" score (1-5): how well the player fits Bill Self's Hi-Lo system
#   5 = ideal archetype  |  1 = poor fit
#
# Portal targets are stratified by position need.
# Salary estimates reflect anticipated NIL range for 2026-27 market.

portal_targets <- tribble(
  ~name,                  ~pos,   ~ht,    ~from_school,            ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal_est, ~value_tier,  ~self_fit, ~notes,

  # ---- BIG MEN (Hi position — post scorer, rebounder, shot blocker) ----
  "Kam Jones",            "C",    "6-11", "Marquette",             120,    95,    7.1,   3.8,   3.3,   0.24,   0.138,    0.000,    0.00,      0.630,   0.010,    0.120,   2200000,      "premium",    5,        "Elite rim-runner, near-perfect ORtg in hi-lo, exactly Bidunga role if Bidunga leaves",
  "Johni Broome",         "PF/C", "6-10", "Auburn",                122,    93,    8.4,   4.2,   4.2,   0.26,   0.145,    0.315,    0.22,      0.635,   0.018,    0.095,   2600000,      "premium",    5,        "Best available big; stretch-capable, elite defensive anchor; may test NBA first",
  "Rubin Jones",          "C",    "6-10", "Cincinnati",            116,    97,    5.2,   2.9,   2.3,   0.21,   0.122,    0.000,    0.00,      0.615,   0.012,    0.108,   1100000,      "value",      5,        "Undervalued at mid-major level; putback scorer, protects rim, fits lo-post anchor",
  "Darius Acuff Jr.",     "PF",   "6-8",  "Tennessee",             114,    98,    4.1,   2.5,   1.6,   0.19,   0.098,    0.340,    0.30,      0.590,   0.016,    0.062,    950000,      "value",      4,        "Stretch 4 who can guard 3-5; adds floor spacing Self's 4-man rarely gets",
  "Zach Edey (grad equiv)","C",   "7-4",  "Purdue",                119,    99,    5.8,   3.1,   2.7,   0.22,   0.160,    0.000,    0.00,      0.625,   0.008,    0.085,   1800000,      "value",      4,        "Archetype: dominant post, limits transition defense; system-dependent",
  "Brandon Garrison",     "C",    "6-11", "Michigan",              113,    100,   3.8,   2.1,   1.7,   0.18,   0.110,    0.000,    0.00,      0.580,   0.009,    0.095,    800000,      "deep_value", 4,        "Rim presence, elite block rate; raw but high ceiling in structured offense",

  # ---- WINGS / FORWARDS (3-4 position, must guard, can shoot) ----
  "RJ Luis Jr.",          "SF",   "6-8",  "UConn",                 118,    96,    5.5,   3.2,   2.3,   0.20,   0.062,    0.362,    0.44,      0.600,   0.020,    0.045,   1400000,      "premium",    5,        "Versatile, high IQ, defends 2-4; exactly what Self recruits at the 3; elite value",
  "Tre Carroll",          "SF",   "6-7",  "Wake Forest",           115,    99,    3.8,   2.3,   1.5,   0.17,   0.055,    0.388,    0.50,      0.585,   0.015,    0.030,    700000,      "value",      4,        "Deep 3 specialist, above avg defender; perfect value rotation wing for $700K",
  "Miles Byrd",           "SG/SF","6-6",  "San Diego St.",         116,    97,    4.4,   2.8,   1.6,   0.19,   0.048,    0.375,    0.48,      0.598,   0.022,    0.028,    900000,      "value",      4,        "Elite defender for position; tournament-tested; Self would love his motor",
  "Adou Thiero",          "SF",   "6-7",  "Arkansas",              112,    100,   2.9,   1.8,   1.1,   0.16,   0.068,    0.342,    0.40,      0.570,   0.018,    0.035,    550000,      "deep_value", 4,        "Athletic, active hands, high ORB rate for a wing; potential breakout year",
  "Boogie Fland",         "SG",   "6-3",  "Arkansas",              117,    98,    4.9,   3.0,   1.9,   0.22,   0.035,    0.360,    0.42,      0.595,   0.024,    0.015,   1200000,      "value",      4,        "Scoring 2-guard with handle; defensive rating excellent; undervalued if Arkansas rebuilds",

  # ---- GUARDS (primary ball handlers / scoring guards) ----
  "Devin Carter",         "PG",   "6-3",  "Providence",            119,    95,    6.2,   3.5,   2.7,   0.26,   0.032,    0.358,    0.38,      0.610,   0.030,    0.012,   1600000,      "premium",    5,        "Elite two-way guard; advanced steal rate, can push in transition; Self's ideal PG type",
  "Johnell Davis",        "SG",   "6-4",  "FAU",                   121,    97,    6.8,   4.1,   2.7,   0.28,   0.038,    0.372,    0.44,      0.625,   0.028,    0.018,   1500000,      "value",      5,        "HUGE deep value; top-10 ortg nationally at FAU, likely underpriced vs. bigger programs",
  "Tre Johnson",          "SG",   "6-5",  "Texas",                 118,    100,   5.1,   3.8,   1.3,   0.28,   0.030,    0.368,    0.48,      0.618,   0.018,    0.010,   2000000,      "premium",    3,        "Elite scorer but defensive metrics are a concern; Self will demand buy-in on D",
  "Tamar Bates",          "SG",   "6-5",  "Indiana",               114,    99,    3.6,   2.4,   1.2,   0.20,   0.042,    0.362,    0.46,      0.588,   0.020,    0.015,    750000,      "value",      4,        "Crafty scorer, IQ player; high 3-pt rate fits Self's spacing scheme",
  "Nijel Pack",           "PG",   "6-0",  "Miami",                 116,    98,    4.2,   2.9,   1.3,   0.22,   0.028,    0.385,    0.50,      0.600,   0.026,    0.008,    900000,      "value",      4,        "Elite catch-and-shoot; runs off screens perfectly; low salary relative to production",
  "CJ Fredrick",          "SG",   "6-4",  "Iowa",                  118,    100,   3.8,   2.9,   0.9,   0.16,   0.025,    0.415,    0.62,      0.618,   0.014,    0.008,    600000,      "deep_value", 3,        "Elite 3pt shooter; limited athleticism for top-10 team but floor spacing value high"
)

# -----------------------------------------------------------------------------
# BUDGET PARAMETERS
# -----------------------------------------------------------------------------
budget_params <- list(
  scenario_a_total   = 8000000,   # Bidunga stays; ~$8M total for new/retained salary
  scenario_b_total   = 10500000,  # Bidunga leaves; recoup ~$2.8M + still have $8M base
  # Committed costs (already on books regardless of scenario)
  committed_a = sum(kansas_returning$nir_sal) + sum(kansas_freshmen$nir_sal),
  committed_b = sum(kansas_returning$nir_sal[kansas_returning$name != "Flory Bidunga"]) +
                sum(kansas_freshmen$nir_sal),
  # These are the $$ remaining for portal acquisitions
  portal_budget_a    = 8000000 - (sum(kansas_returning$nir_sal) + sum(kansas_freshmen$nir_sal)),
  portal_budget_b    = 10500000 - (
    sum(kansas_returning$nir_sal[kansas_returning$name != "Flory Bidunga"]) +
    sum(kansas_freshmen$nir_sal)
  )
)

# Quick check
message("Scenario A — Portal Budget Available: $",
        format(budget_params$portal_budget_a, big.mark=","))
message("Scenario B — Portal Budget Available: $",
        format(budget_params$portal_budget_b, big.mark=","))
