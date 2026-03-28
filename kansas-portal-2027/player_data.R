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
#   ortg     = offensive rating (points per 100 possessions)
#   drtg     = defensive rating (points allowed per 100 while on floor)
#   bpm      = box plus/minus per 100 possessions
#   obpm     = offensive BPM
#   dbpm     = defensive BPM
#   usage    = usage rate (% of team possessions used)
#   orb_pct  = offensive rebound rate
#   fg3_pct  = 3-point percentage
#   fg3_rate = 3-point attempt rate (3PA / FGA)
#   ts_pct   = true shooting %
#   stl_pct  = steal rate
#   blk_pct  = block rate
#   nir_sal  = estimated NIL salary (USD) for 2026-27 negotiation

kansas_returning <- tribble(
  ~name,                ~pos,  ~ht,    ~yr_26_27, ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal,
  # Flory Bidunga — Scenario A only; removed from pool in Scenario B
  "Flory Bidunga",      "C",   "6-10", "Jr",       118,   96,    6.8,   3.2,   3.6,   0.22,   0.128,    0.000,    0.00,      0.620,   0.012,    0.110,    2800000,
  # Elmarko Jackson — combo guard, transferred from South Carolina; athletic scorer,
  # defensive disruptor; Self's system should unlock his scoring off-ball
  "Elmarko Jackson",    "G",   "6-3",  "Sr",       113,   99,    2.5,   1.8,   0.7,   0.22,   0.030,    0.345,    0.40,      0.575,   0.022,    0.012,     500000,
  # Jamari McDowell — versatile forward, length and athleticism to guard 3-5;
  # developing offensive game; high-upside piece in Self's Hi-Man rotation
  "Jamari McDowell",    "SF",  "6-7",  "Jr",       110,   100,   2.0,   1.3,   0.7,   0.16,   0.060,    0.340,    0.38,      0.565,   0.018,    0.022,     350000
)

# -----------------------------------------------------------------------------
# INCOMING FRESHMEN — Kansas 2026-27 Class
# -----------------------------------------------------------------------------
# NIL estimates inferred from recruiting rank tier, position scarcity, and
# Kansas market context. 2026-27 freshman NIL market has grown ~18% vs 2024-25.
#
# Taylen Kinney (TK)  — top-15 national recruit; stretch-4/5 with elite upside;
#   $1.5M reflects high-end 5-star market rate for impact big men
# Trent Perry         — top-40 combo guard; perimeter creator with size; could
#   contribute early as a scoring option off the bench
# Davion Adkins       — top-50 wing/forward; athletic, plays both ends; fits
#   Self's 3-man role with natural defensive instincts
# Luke Barnett        — top-60 big; length and shot-blocking upside; likely
#   develops behind Bidunga (Scenario A) or slides into rotation (Scenario B)

kansas_freshmen <- tribble(
  ~name,              ~pos,   ~ht,    ~yr_26_27, ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal, ~recruit_tier,
  "Taylen Kinney",    "PF",   "6-9",  "Fr",       109,   101,   2.1,   1.4,   0.7,   0.19,   0.090,    0.320,    0.28,      0.545,   0.015,    0.060,    1200000,  "5-star / Top 15",
  "Trent Perry",      "G",    "6-4",  "Fr",       106,   102,   1.2,   0.9,   0.3,   0.18,   0.028,    0.348,    0.44,      0.535,   0.020,    0.010,     400000,  "4-star / Top 40",
  "Davion Adkins",    "SF",   "6-7",  "Fr",       105,   102,   0.9,   0.6,   0.3,   0.15,   0.052,    0.332,    0.38,      0.525,   0.018,    0.018,     300000,  "4-star / Top 50",
  "Luke Barnett",     "C",    "6-11", "Fr",       103,   103,   0.4,   0.2,   0.2,   0.13,   0.095,    0.000,    0.00,      0.530,   0.010,    0.065,     200000,  "4-star / Top 60"
)

# -----------------------------------------------------------------------------
# TRANSFER PORTAL TARGETS — 2026 Pool
# -----------------------------------------------------------------------------
# "value_tier":
#   "premium"    = known star, market-rate or above
#   "value"      = strong metrics, likely underpriced due to school/fit
#   "deep_value" = high efficiency in limited role; upside in right system
#
# "self_fit" (1-5): fit for Bill Self Hi-Lo
#   5 = ideal archetype  |  1 = poor fit
#
# Assume all players are available unless confirmed otherwise.

portal_targets <- tribble(
  ~name,                    ~pos,    ~ht,    ~from_school,         ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal_est, ~value_tier,  ~self_fit, ~notes,

  # ===========================================================================
  # CENTERS / POWER FORWARDS — Lo Man & Hi Man candidates
  # ===========================================================================

  "Johni Broome",           "PF/C",  "6-10", "Auburn",             122,   93,    8.4,   4.2,   4.2,   0.26,   0.145,    0.315,    0.22,      0.635,   0.018,    0.095,   2600000, "premium",    5, "Best available big nationally; elite two-way anchor; defensive force in Hi-Lo; may test NBA first but worth pursuing hard",
  "Kam Jones",              "C",     "6-11", "Marquette",          120,   95,    7.1,   3.8,   3.3,   0.24,   0.138,    0.000,    0.00,      0.630,   0.010,    0.120,   2200000, "premium",    5, "Elite rim-runner, near-perfect ORtg in Hi-Lo system; blocks shots, catches lobs; exactly the Bidunga mold",
  "Felix Okpara",           "C",     "6-11", "Ohio State",         115,   94,    5.6,   2.4,   3.2,   0.18,   0.118,    0.000,    0.00,      0.590,   0.010,    0.135,   1400000, "value",      5, "Elite shot blocker (top-5 nationally in blk%); limited offensively but rim-to-rim force; ideal Lo Man backup/rotation",
  "Trevon Brazile",         "PF",    "6-9",  "Arkansas",           116,   97,    5.0,   3.0,   2.0,   0.20,   0.120,    0.310,    0.25,      0.595,   0.020,    0.080,   1200000, "value",      5, "Ultra-athletic forward with elite ORB rate and motor; defensive awareness for position; Hi-Man with upside",
  "Malik Reneau",           "PF",    "6-9",  "Indiana",            117,   98,    4.8,   3.2,   1.6,   0.22,   0.105,    0.325,    0.28,      0.600,   0.015,    0.055,   1100000, "value",      5, "Skilled interior scorer; high-low IQ, reads doubles well; solid rebounder who can step out to mid-range",
  "Rubin Jones",            "C",     "6-10", "Cincinnati",         116,   97,    5.2,   2.9,   2.3,   0.21,   0.122,    0.000,    0.00,      0.615,   0.012,    0.108,   1100000, "value",      5, "Undervalued mid-major big; putback scorer, protects rim, elite blk%; $700K-$1.5M below equivalent P4 big",
  "Fousseyni Traore",       "PF/C",  "6-10", "Minnesota",          113,   98,    4.0,   2.2,   1.8,   0.19,   0.130,    0.000,    0.00,      0.575,   0.012,    0.075,    850000, "value",      4, "Relentless rebounder; elite ORB rate, physical post presence; limited range but excellent in structured Hi-Lo",
  "Darius Acuff Jr.",       "PF",    "6-8",  "Tennessee",          114,   98,    4.1,   2.5,   1.6,   0.19,   0.098,    0.340,    0.30,      0.590,   0.016,    0.062,    950000, "value",      4, "Stretch 4 who can guard 3-5; rare Hi-Man who spaces floor; Tennessee defense pedigree translates to Self system",
  "Braxton Meah",           "C",     "7-0",  "Washington",         110,   100,   3.2,   1.6,   1.6,   0.17,   0.108,    0.000,    0.00,      0.570,   0.008,    0.105,    700000, "deep_value", 4, "Raw but physical; excellent block rate, crashes glass hard; needs structured offensive system to maximize — Hi-Lo ideal",
  "Yanic Konan Niederhauser","C",    "7-1",  "Utah State",         112,   98,    4.2,   2.2,   2.0,   0.18,   0.112,    0.285,    0.18,      0.585,   0.008,    0.118,    800000, "value",      4, "Massive shot blocker with some stretch; mid-major discount applies; physical tools project well in Big 12",
  "Brandon Garrison",       "C",     "6-11", "Michigan",           113,   100,   3.8,   2.1,   1.7,   0.18,   0.110,    0.000,    0.00,      0.580,   0.009,    0.095,    800000, "deep_value", 4, "Rim presence, elite block rate; raw but high ceiling in structured offense like Hi-Lo",
  "Aday Mara",              "C",     "7-3",  "Charlotte",          108,   101,   3.0,   1.5,   1.5,   0.15,   0.098,    0.000,    0.00,      0.555,   0.006,    0.125,    550000, "deep_value", 3, "Extreme length and shot-blocking upside; extremely raw offensively; developmental big who could emerge as rotational piece",
  "Thomas Haugh",           "PF",    "6-9",  "Duke",               113,   99,    3.5,   2.1,   1.4,   0.17,   0.085,    0.350,    0.35,      0.582,   0.014,    0.042,    900000, "value",      4, "Skilled, high-IQ forward from Duke system; can stretch floor with 3P shot; above-avg defender, Smart Hi-Man type",

  # ===========================================================================
  # WINGS / SMALL FORWARDS — 3-and-D, must guard 2-4
  # ===========================================================================

  "RJ Luis Jr.",            "SF",    "6-8",  "UConn",              118,   96,    5.5,   3.2,   2.3,   0.20,   0.062,    0.362,    0.44,      0.600,   0.020,    0.045,   1400000, "premium",    5, "Versatile, high IQ, defends 2-4; exactly what Self recruits at the 3; tourney-tested in high-stakes system",
  "Cam Christie",           "SF",    "6-7",  "UCLA",               115,   98,    4.0,   2.6,   1.4,   0.18,   0.050,    0.378,    0.46,      0.590,   0.016,    0.028,    950000, "value",      4, "Long-armed wing with shooting ability; good defensive instincts, can guard multiple positions; undervalued at UCLA rebuild",
  "Kobe Johnson",           "SF",    "6-7",  "USC",                113,   97,    3.6,   2.0,   1.6,   0.17,   0.055,    0.355,    0.42,      0.578,   0.022,    0.035,    800000, "value",      4, "High-end defensive wing; SDSU-pedigree defender, active hands, excellent rotation instincts; fits 3-and-D mold perfectly",
  "Miles Byrd",             "SG/SF", "6-6",  "San Diego St.",      116,   97,    4.4,   2.8,   1.6,   0.19,   0.048,    0.375,    0.48,      0.598,   0.022,    0.028,    900000, "value",      4, "Tournament-tested; elite defender for position; SDSU pedigree means he's learned to guard at the highest level",
  "Jaxson Robinson",        "SG/SF", "6-6",  "Arkansas",           114,   99,    3.5,   2.4,   1.1,   0.18,   0.045,    0.372,    0.50,      0.588,   0.018,    0.020,    750000, "value",      4, "Elite 3P shooter at high rate; heady player in structured systems; Arkansas tenure means he can handle Big 12 physicality",
  "Adou Thiero",            "SF",    "6-7",  "Arkansas",           112,   100,   2.9,   1.8,   1.1,   0.16,   0.068,    0.342,    0.40,      0.570,   0.018,    0.035,    550000, "deep_value", 4, "Athletic, active hands, above-avg ORB rate for a wing; defensive upside; could emerge in role-defined system",
  "Andrej Stojakovic",      "SG",    "6-6",  "Stanford",           116,   100,   3.8,   2.8,   1.0,   0.19,   0.038,    0.390,    0.52,      0.598,   0.016,    0.015,    800000, "value",      4, "High-IQ shooter, son of Peja; elite 3P% at high rate, understands spacing; below-market due to Stanford profile",
  "Keshad Johnson",         "SG/SF", "6-6",  "Arizona",            113,   98,    3.2,   1.8,   1.4,   0.16,   0.052,    0.352,    0.42,      0.575,   0.022,    0.032,    700000, "value",      4, "High-energy defensive specialist; elite motor, multiple steals/blocks per game; exactly Self's 3-man type",
  "Silas Demary Jr.",       "SG/SF", "6-6",  "Georgia Tech",       114,   101,   3.0,   2.1,   0.9,   0.20,   0.042,    0.358,    0.44,      0.580,   0.020,    0.018,    650000, "deep_value", 4, "Athletic wing with scoring upside; defensive tools are there, needs right system; ACC experience in physical play",
  "Tre Carroll",            "SF",    "6-7",  "Wake Forest",        115,   99,    3.8,   2.3,   1.5,   0.17,   0.055,    0.388,    0.50,      0.585,   0.015,    0.030,    700000, "value",      4, "Pure 3-and-D wing; 38.8% from 3 at very high rate; solid defensive metrics; high value per dollar at $700K",
  "Boogie Fland",           "SG",    "6-3",  "Arkansas",           117,   98,    4.9,   3.0,   1.9,   0.22,   0.035,    0.360,    0.42,      0.595,   0.024,    0.015,   1200000, "value",      4, "Scoring 2-guard with handle and defensive chops; undervalued if Arkansas enters rebuild; excellent steal rate",

  # ===========================================================================
  # GUARDS — Lead guards & scoring guards
  # ===========================================================================

  "Devin Carter",           "PG",    "6-3",  "Providence",         119,   95,    6.2,   3.5,   2.7,   0.26,   0.032,    0.358,    0.38,      0.610,   0.030,    0.012,   1600000, "premium",    5, "Elite two-way guard; 3.0% steal rate is top-tier nationally; pushes transition, runs half-court; Self's ideal PG archetype",
  "Johnell Davis",          "SG",    "6-4",  "FAU",                121,   97,    6.8,   4.1,   2.7,   0.28,   0.038,    0.372,    0.44,      0.625,   0.028,    0.018,   1500000, "value",      5, "STANDOUT undervalue: top-20 ORtg nationally at 28% usage; mid-major discount = $400-700K below P4 equivalent; must target",
  "RJ Davis",               "G",     "6-1",  "North Carolina",     118,   98,    4.8,   3.2,   1.6,   0.24,   0.028,    0.378,    0.50,      0.608,   0.024,    0.010,   1300000, "premium",    4, "Battle-tested veteran guard; ACC champion-level toughness; excellent 3P%, high-volume shot creator; leadership value",
  "Chucky Hepburn",         "PG",    "6-1",  "Wisconsin",          115,   97,    4.0,   2.5,   1.5,   0.22,   0.025,    0.365,    0.44,      0.592,   0.028,    0.008,   1000000, "value",      5, "High-IQ, disciplined PG; Wisconsin defensive pedigree matches Self's standards; runs sets, minimal turnovers, heady",
  "Dug McDaniel",           "PG",    "6-0",  "Michigan",           116,   99,    3.8,   2.8,   1.0,   0.25,   0.022,    0.352,    0.38,      0.588,   0.026,    0.008,    900000, "value",      4, "Speedster PG with excellent vision; pushes pace perfectly for Self's transition game; solid defensive effort",
  "Jeremiah Fears",         "G",     "6-4",  "Oklahoma",           115,   100,   3.5,   2.6,   0.9,   0.24,   0.030,    0.345,    0.40,      0.585,   0.022,    0.012,    950000, "value",      4, "Young, athletic scorer with upside; shows big-moment mentality; needs defensive consistency — Self will push that hard",
  "Ian Jackson",            "SG",    "6-4",  "North Carolina",     117,   99,    4.5,   3.2,   1.3,   0.23,   0.035,    0.362,    0.44,      0.600,   0.020,    0.015,   1100000, "value",      4, "Elite athlete with scoring upside; long, can guard 2-3; UNC background = understands winning culture and system play",
  "Elliot Cadeau",          "PG",    "6-2",  "North Carolina",     113,   99,    3.0,   2.0,   1.0,   0.21,   0.025,    0.338,    0.38,      0.578,   0.026,    0.010,    800000, "value",      4, "Young but polished PG; excellent assist-to-turnover ratio; defensive activity level very high for the position",
  "Javian McCollum",        "G",     "6-2",  "South Carolina",     116,   100,   3.6,   2.6,   1.0,   0.24,   0.028,    0.358,    0.44,      0.592,   0.022,    0.010,    750000, "value",      3, "Smooth scoring guard; good shot maker in half-court; defensive effort needs consistency for Self's standard",
  "Caleb Love",             "SG",    "6-4",  "Arizona",            115,   101,   3.2,   2.6,   0.6,   0.26,   0.030,    0.352,    0.46,      0.582,   0.018,    0.010,   1200000, "premium",    3, "Elite scorer and name brand; high usage, big shot maker; defensive metrics are borderline for Self — culture fit is key question",
  "Tamar Bates",            "SG",    "6-5",  "Indiana",            114,   99,    3.6,   2.4,   1.2,   0.20,   0.042,    0.362,    0.46,      0.588,   0.020,    0.015,    750000, "value",      4, "Crafty off-ball scorer, high 3P rate fits spacing scheme; IQ player who runs actions well; solid defensive awareness",
  "Nijel Pack",             "PG",    "6-0",  "Miami",              116,   98,    4.2,   2.9,   1.3,   0.22,   0.028,    0.385,    0.50,      0.600,   0.026,    0.008,    900000, "value",      4, "Elite catch-and-shoot; runs off screens, exactly what Hi-Lo needs; mid-major discount makes him $300-500K below market",
  "Tre Johnson",            "SG",    "6-5",  "Texas",              118,   100,   5.1,   3.8,   1.3,   0.28,   0.030,    0.368,    0.48,      0.618,   0.018,    0.010,   2000000, "premium",    3, "Elite scorer, likely highest ORtg on any roster; defensive metrics a concern — Self will demand full buy-in on D",
  "CJ Fredrick",            "SG",    "6-4",  "Iowa",               118,   100,   3.8,   2.9,   0.9,   0.16,   0.025,    0.415,    0.62,      0.618,   0.014,    0.008,    600000, "deep_value", 3, "Elite 3P shooter at very high rate (62% of shots are 3s); limited athleticism but floor spacing value is real"
)

# -----------------------------------------------------------------------------
# BUDGET PARAMETERS
# -----------------------------------------------------------------------------
budget_params <- list(
  scenario_a_total  = 8000000,   # Bidunga stays; ~$8M total roster NIL
  scenario_b_total  = 10500000,  # Bidunga leaves; ~$2.8M recouped + $8M base

  # Committed costs in each scenario
  committed_a = sum(kansas_returning$nir_sal) + sum(kansas_freshmen$nir_sal),
  committed_b = sum(kansas_returning$nir_sal[kansas_returning$name != "Flory Bidunga"]) +
                sum(kansas_freshmen$nir_sal),

  # Remaining for portal acquisitions
  portal_budget_a = 8000000 -
    (sum(kansas_returning$nir_sal) + sum(kansas_freshmen$nir_sal)),
  portal_budget_b = 10500000 - (
    sum(kansas_returning$nir_sal[kansas_returning$name != "Flory Bidunga"]) +
    sum(kansas_freshmen$nir_sal)
  )
)

message("Scenario A — Portal Budget Available: $",
        format(budget_params$portal_budget_a, big.mark = ","))
message("Scenario B — Portal Budget Available: $",
        format(budget_params$portal_budget_b, big.mark = ","))
