# =============================================================================
# Kansas Basketball 2026-27 Portal Analysis — Player Database
# Stats updated to reflect 2025-26 season actuals (BartTorvik / EvanMiya)
# Last updated: March 2026
# =============================================================================

library(dplyr)
library(tibble)

# -----------------------------------------------------------------------------
# RETURNING KANSAS PLAYERS
# -----------------------------------------------------------------------------
# Stats reflect 2025-26 season actuals where available.
# Bidunga: Big 12 Defensive Player of the Year; decision pending (late 1st /
#   early 2nd round projection). Included in Scenario A only.
# Jackson: Returned from torn patellar tendon (missed all of 2024-25); limited
#   role player (4.9 PPG, 18 MPG) but system contributor.
# McDowell: 6'5" combo guard (not SF as previously listed); 3.8 PPG in
#   rotation role; improved 3P shooting (35.3%).

kansas_returning <- tribble(
  ~name,              ~pos, ~ht,    ~yr_26_27, ~ortg, ~drtg, ~bpm, ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal,
  # Scenario A only. Big 12 Def. POY. 13.3 PPG / 9.0 RPG / 2.6 BPG.
  # Tankathon: #4 among 2026 draft centers. Decision pending.
  "Flory Bidunga",    "C",  "6-10", "Jr",       120,   94,    7.8,   3.4,   4.4,   0.23,   0.138,    0.000,    0.00,      0.635,   0.012,    0.115,   3200000,
  # Returned from patellar tendon tear. 4.9 PPG / 37.0% FG / 37.2% 3P.
  # Career-high 19 pts at K-State. Upside remains but durability is a question.
  "Elmarko Jackson",  "G",  "6-3",  "Sr",       110,   100,   1.8,   1.2,   0.6,   0.17,   0.025,    0.372,    0.42,      0.560,   0.019,    0.010,    500000,
  # 3.8 PPG / 35.3% 3P in rotation role. 6'5" combo guard (prev. listed as SF).
  # Emerged in rotation when Peterson was injured.
  "Jamari McDowell",  "G",  "6-5",  "Jr",       107,   101,   1.5,   1.0,   0.5,   0.14,   0.030,    0.353,    0.40,      0.548,   0.016,    0.012,    350000
)

# -----------------------------------------------------------------------------
# INCOMING FRESHMEN — Kansas 2026-27 Class (No. 1 nationally per 247Sports)
# -----------------------------------------------------------------------------
# POSITION CORRECTIONS from prior version:
#   Taylen Kinney  = 6'1" PG  (was incorrectly listed as 6'9" PF)
#   Davion Adkins  = 6'9" C   (was incorrectly listed as 6'7" SF)
#   Luke Barnett   = 6'4" SG  (was incorrectly listed as 6'11" C)
#   Trent Perry    = 6'5" SF  (unchanged)
#
# NIL estimates updated to reflect correct positions and recruit tiers.
# Freshman stat projections discounted ~10-12 pts ORtg vs. upperclassmen.

kansas_freshmen <- tribble(
  ~name,            ~pos, ~ht,    ~yr_26_27, ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal, ~recruit_tier,
  # 6'1" PG from Newport KY / Overtime Elite. #13-17 nationally (247, ESPN).
  # Smooth combo guard; excellent pick-and-roll feel; expected to take over
  # lead guard duties. Chose KU over Kentucky, Arkansas, Indiana.
  "Taylen Kinney",  "PG", "6-1",  "Fr",       108,   101,   1.9,   1.4,   0.5,   0.20,   0.022,    0.355,    0.44,      0.548,   0.022,    0.008,   1200000, "5-star / Top 17",
  # 6'5" SF from Link Academy. 7'1" wingspan. #105-120 nationally.
  # Athletic, physically built wing; defensive specialist; elite motor.
  # First to sign in early signing period. Fits Self's 3-man mold.
  "Trent Perry",    "SF", "6-5",  "Fr",       104,   102,   0.9,   0.6,   0.3,   0.14,   0.048,    0.330,    0.36,      0.520,   0.020,    0.018,    400000, "4-star / Top 110",
  # 6'9" C from Prolific Prep. 7'2" wingspan / 9'0" standing reach. #30-34 nationally.
  # Explosive lob threat, shot-blocker, high-% finisher (58.5% FG in HS/AAU).
  # Shifts Hi-Man/Lo-Man depth chart — fits Lo Man or Hi-Man depending on lineup.
  "Davion Adkins",  "C",  "6-9",  "Fr",       106,   102,   1.0,   0.6,   0.4,   0.14,   0.090,    0.000,    0.00,      0.545,   0.010,    0.068,    700000, "4-star / Top 34",
  # 6'4" SG from Mater Dei (Santa Ana, CA). Elite shooter. #113-144 nationally.
  # 37% from 3 on Nike EYBL circuit; quick trigger off catch; deep range.
  # His commitment pushed KU to #1 nationally. Provides immediate floor spacing.
  "Luke Barnett",   "SG", "6-4",  "Fr",       105,   103,   0.5,   0.4,   0.1,   0.13,   0.022,    0.378,    0.58,      0.542,   0.012,    0.006,    250000, "4-star / Top 130"
)

# -----------------------------------------------------------------------------
# TRANSFER PORTAL TARGETS — 2026 Pool
# -----------------------------------------------------------------------------
# MAJOR REFRESH vs. prior version. Many previously listed players are no longer
# available (drafted, G League, pro). See inline notes.
#
# Removed (NBA / G League / eligibility exhausted):
#   Johni Broome (Sixers), Kam Jones (Pacers), Devin Carter (Kings, 2024 pick),
#   Tre Johnson (Wizards), Jeremiah Fears (Pelicans), Chucky Hepburn (Raptors),
#   Johnell Davis (G League), RJ Davis (G League), Caleb Love (Blazers),
#   Kobe Johnson (G League), Cam Christie (Clippers), RJ Luis Jr. (Jazz org),
#   Adou Thiero (Lakers), Jaxson Robinson (G League), Braxton Meah (eligible done),
#   Fousseyni Traore (France pro), Darius Acuff Jr. (top-3 2026 draft pick)
#
# Removed (confirmed returning to current school):
#   Boogie Fland (announced returning to Florida for 2026-27)
#
# "draft_risk" flag: HIGH = likely declaring; MEDIUM = possible; LOW = unlikely
# "availability" flag: LIKELY | UNCERTAIN | DRAFT_RISK

portal_targets <- tribble(
  ~name,                    ~pos,    ~ht,    ~from_school,    ~ortg, ~drtg, ~bpm,  ~obpm, ~dbpm, ~usage, ~orb_pct, ~fg3_pct, ~fg3_rate, ~ts_pct, ~stl_pct, ~blk_pct, ~nir_sal_est, ~value_tier,  ~self_fit, ~draft_risk, ~notes,

  # ===========================================================================
  # CENTERS / POWER FORWARDS
  # ===========================================================================

  # VERIFIED 2025-26: Transferred from Ohio St to Tennessee for 2024-25 and stayed.
  # 7.8 PPG / 6.2 RPG / SEC All-Defensive Team. Elite shot blocker (top-5 nationally
  # in blk%). Limited offensive role but projectable in Hi-Lo with more usage.
  "Felix Okpara",           "C",     "6-11", "Tennessee",     113,   93,    4.8,   1.8,   3.0,   0.15,   0.112,    0.000,    0.00,      0.580,   0.010,    0.138,   1300000, "value",      5, "LOW",    "Best available big — elite rim protection (SEC All-Def Team); limited offensively but projects well in structured Hi-Lo system",

  # VERIFIED 2025-26: Transferred from Indiana to Miami. ALL-ACC First Team.
  # 19.0 PPG / 6.6 RPG / 55.9% FG / 36.5% 3P. Breakout season.
  # HIGH draft risk — nearly certain to declare for 2026 NBA draft.
  "Malik Reneau",           "PF",    "6-9",  "Miami (FL)",    120,   97,    6.2,   4.1,   2.1,   0.26,   0.108,    0.365,    0.30,      0.620,   0.015,    0.058,   2000000, "premium",    5, "HIGH",   "Breakout year (19 PPG / All-ACC); skilled Hi-Man with range; almost certainly declaring for 2026 draft — pursue only if he withdraws",

  # VERIFIED 2025-26: Arkansas senior. 13.2 PPG / 7.4 RPG / 53.4% FG / 36.0% 3P.
  # Career-high 28 pts vs. Texas. Sweet 16. ESPN Big Board: ~#81. Final year.
  # Some draft interest but scouts lukewarm. Could return if undrafted.
  "Trevon Brazile",         "PF",    "6-9",  "Arkansas",      116,   97,    5.2,   3.2,   2.0,   0.22,   0.118,    0.360,    0.28,      0.600,   0.020,    0.078,   1400000, "value",      5, "MEDIUM", "Final year; elite motor and ORB rate; can guard 3-5; borderline draft prospect — could be available if undrafted/withdraws",

  # NOT VERIFIED 2025-26 (unconfirmed school/availability). Prior data had him
  # at Cincinnati. Assume still active — flag as uncertain.
  "Rubin Jones",            "C",     "6-10", "Cincinnati",    116,   97,    5.2,   2.9,   2.3,   0.21,   0.122,    0.000,    0.00,      0.615,   0.012,    0.108,   1100000, "value",      5, "LOW",    "UNCERTAIN AVAILABILITY — verify enrollment. If available: mid-major big with elite ORB/blk%; near-Bidunga profile at fraction of cost",

  # VERIFIED 2025-26: At Kentucky (NOT Michigan as previously listed).
  # 4.7 PPG / 4.1 RPG in limited role under Mark Pope. McDonald's AA.
  # Year 2 jump expected per coaching staff. Available if he enters portal.
  "Brandon Garrison",       "C",     "6-11", "Kentucky",      111,   100,   3.2,   1.8,   1.4,   0.16,   0.108,    0.000,    0.00,      0.568,   0.008,    0.092,    700000, "deep_value", 4, "LOW",    "At Kentucky (corrected from Michigan). Limited role but McDonald's AA upside; Year 2 jump candidate; available if he seeks more minutes",

  # NOT VERIFIED 2025-26. Assume still at Duke. Stretch-4 who can guard 3-5.
  "Thomas Haugh",           "PF",    "6-9",  "Duke",          113,   99,    3.5,   2.1,   1.4,   0.17,   0.085,    0.350,    0.35,      0.582,   0.014,    0.042,    900000, "value",      4, "LOW",    "UNVERIFIED — assume active. High-IQ Duke-system forward; can stretch floor; above-avg defender; smart Hi-Man type",

  # NOT VERIFIED 2025-26. Prior data had him at Utah State.
  "Yanic Konan Niederhauser","C",    "7-1",  "Utah State",    112,   98,    4.2,   2.2,   2.0,   0.18,   0.112,    0.285,    0.18,      0.585,   0.008,    0.118,    750000, "value",      4, "LOW",    "UNVERIFIED — assume active. Massive shot blocker with some stretch; mid-major discount applies; physical tools project well in Big 12",

  # ===========================================================================
  # WINGS / SMALL FORWARDS
  # ===========================================================================

  # VERIFIED 2025-26: Returned to SDSU after withdrawing from 2025 draft.
  # 10.4 PPG / 4.7 RPG. Mountain West Defensive Player of the Year.
  # WARNING: 3P% dropped to 30.8% this year (was projected at 37.5%).
  # Expected to declare for 2026 draft as late 2nd / undrafted prospect.
  "Miles Byrd",             "SG/SF", "6-7",  "San Diego St.", 113,   96,    4.0,   2.2,   1.8,   0.18,   0.048,    0.308,    0.44,      0.578,   0.024,    0.030,    700000, "value",      4, "MEDIUM", "MWC Def. POY — elite defensive wing; WARNING: 3P% dropped to 30.8% (was 37.5%). Draft risk. Fit grade drops without reliable shooting",

  # NOT VERIFIED 2025-26. Assume still at Arizona or transferred.
  "Keshad Johnson",         "SG/SF", "6-6",  "Arizona",       113,   98,    3.2,   1.8,   1.4,   0.16,   0.052,    0.352,    0.42,      0.575,   0.022,    0.032,    700000, "value",      4, "LOW",    "UNVERIFIED — check portal status. High-energy defensive specialist; exactly Self's 3-man type; above-avg ORB rate for wing",

  # NOT VERIFIED 2025-26. Assume still at Wake Forest.
  "Tre Carroll",            "SF",    "6-7",  "Wake Forest",   115,   99,    3.8,   2.3,   1.5,   0.17,   0.055,    0.388,    0.50,      0.585,   0.015,    0.030,    650000, "value",      4, "LOW",    "UNVERIFIED — check portal status. Pure 3-and-D; 38.8% from 3 at high rate; solid defensive metrics; high value per dollar",

  # NOT VERIFIED 2025-26. Assume still at Stanford.
  "Andrej Stojakovic",      "SG",    "6-6",  "Stanford",      116,   100,   3.8,   2.8,   1.0,   0.19,   0.038,    0.390,    0.52,      0.598,   0.016,    0.015,    750000, "value",      4, "LOW",    "UNVERIFIED — check portal status. Elite 3P shooter (son of Peja); high IQ; below-market due to Stanford profile",

  # NOT VERIFIED 2025-26.
  "Silas Demary Jr.",       "SG/SF", "6-6",  "Georgia Tech",  114,   101,   3.0,   2.1,   0.9,   0.20,   0.042,    0.358,    0.44,      0.580,   0.020,    0.018,    600000, "deep_value", 4, "LOW",    "UNVERIFIED — check portal status. Athletic wing with scoring upside; defensive tools present; ACC experience in physical play",

  # ===========================================================================
  # GUARDS
  # ===========================================================================
  # NOTE: Guard market significantly thinner than projected in prior version.
  # Devin Carter (Kings), Tre Johnson (Wizards), Chucky Hepburn (Raptors),
  # Johnell Davis (G League), RJ Davis (G League), Jeremiah Fears (Pelicans),
  # and Caleb Love (Blazers) are ALL no longer in college.

  # VERIFIED 2025-26: Transferred from UNC to Michigan. Michigan went 34-3,
  # Big Ten champs, No. 1 seed, Elite Eight. 10.1 PPG / 6.2 APG / 37.3% 3P.
  # Sweet 16: 17 pts / 7 ast vs. Alabama. Undersized (6'2") but elite playmaker.
  # Eligible to return — draft risk is moderate (undersized, developing scorer).
  "Elliot Cadeau",          "PG",    "6-2",  "Michigan",      115,   98,    4.0,   2.8,   1.2,   0.22,   0.025,    0.373,    0.42,      0.594,   0.026,    0.008,   1100000, "premium",    5, "MEDIUM", "Best available PG — elite playmaker on Elite 8 team; 6.2 APG, 37.3% 3P; undersized but wins; moderate draft risk; top priority if available",

  # VERIFIED 2025-26: Transferred from UNC to St. John's. St. John's Sweet 16.
  # 9.6 PPG / 35.3% 3P / 90.0% FT. 19 pts / 5 steals vs. Villanova.
  # Beat Kansas 67-65 in Round of 32. Long, athletic SG with scoring upside.
  "Ian Jackson",            "SG",    "6-4",  "St. John's",    115,   99,    4.0,   2.8,   1.2,   0.21,   0.035,    0.353,    0.42,      0.592,   0.022,    0.015,    950000, "value",      4, "LOW",    "Transferred from UNC; 35.3% 3P on good volume; length and athleticism project well; Kansas connection (beat KU in R32); eligible to return",

  # VERIFIED 2025-26: Transferred from Miami to Oklahoma with medical waiver.
  # BREAKOUT: 16.5 PPG / 45.2% 3P / 86.0% FT / 47.4% FG. PER: 20.1.
  # Final year of eligibility. 2026 draft eligible — likely pursuing NBA.
  "Nijel Pack",             "PG",    "6-0",  "Oklahoma",      118,   98,    5.2,   3.8,   1.4,   0.24,   0.025,    0.452,    0.52,      0.618,   0.026,    0.008,   1400000, "value",      5, "HIGH",   "ELITE YEAR: 45.2% 3P / 16.5 PPG. Best shooter available if he doesn't go pro. Final year — almost certainly declaring for 2026 draft. Monitor closely",

  # VERIFIED 2025-26: Transferred from Michigan → Kansas State → Memphis.
  # 13.9 PPG / 4.6 APG / 32.9% 3P / 84.9% FT at Memphis. Final year.
  # Improved significantly from 25.8% 3P at K-State. NYC area product.
  "Dug McDaniel",           "PG",    "6-0",  "Memphis",       114,   100,   3.5,   2.6,   0.9,   0.24,   0.020,    0.329,    0.40,      0.582,   0.024,    0.008,    800000, "value",      4, "LOW",    "Final year at Memphis; 13.9 PPG / 4.6 APG; 3P% (32.9%) a concern for Self's spacing requirement but handles and IQ are real",

  # NOT VERIFIED 2025-26. Assume still at Indiana or transferred.
  "Tamar Bates",            "SG",    "6-5",  "Indiana",       114,   99,    3.6,   2.4,   1.2,   0.20,   0.042,    0.362,    0.46,      0.588,   0.020,    0.015,    700000, "value",      4, "LOW",    "UNVERIFIED — check portal status. Crafty off-ball scorer; high 3P attempt rate fits spacing scheme; solid defensive awareness",

  # NOT VERIFIED 2025-26. Assume still at South Carolina.
  "Javian McCollum",        "G",     "6-2",  "South Carolina",114,   101,   3.2,   2.4,   0.8,   0.23,   0.025,    0.350,    0.42,      0.582,   0.020,    0.008,    650000, "deep_value", 3, "LOW",    "UNVERIFIED. Smooth scoring guard; borderline Self-fit on defense — needs D buy-in",

  # NOT VERIFIED 2025-26. Assume graduated or 5th year somewhere.
  "CJ Fredrick",            "SG",    "6-4",  "Iowa",          118,   100,   3.8,   2.9,   0.9,   0.16,   0.025,    0.415,    0.62,      0.618,   0.014,    0.008,    500000, "deep_value", 3, "LOW",    "UNVERIFIED eligibility status — verify. Elite 3P shooter at very high volume; limited athleticism but floor spacing value real"
)

# -----------------------------------------------------------------------------
# BUDGET PARAMETERS (updated NIL values)
# -----------------------------------------------------------------------------
# Bidunga NIL updated to reflect Big 12 Defensive POY status ($3.2M).
# Adkins NIL updated to reflect top-35 C profile ($700K, not $300K).
# Barnett NIL updated to reflect SG profile ($250K, not $200K).
# Portal budgets recalculated accordingly.

budget_params <- list(
  scenario_a_total  = 8000000,
  scenario_b_total  = 10500000,

  committed_a = sum(kansas_returning$nir_sal) + sum(kansas_freshmen$nir_sal),
  committed_b = sum(kansas_returning$nir_sal[kansas_returning$name != "Flory Bidunga"]) +
                sum(kansas_freshmen$nir_sal),

  portal_budget_a = 8000000 -
    (sum(kansas_returning$nir_sal) + sum(kansas_freshmen$nir_sal)),
  portal_budget_b = 10500000 - (
    sum(kansas_returning$nir_sal[kansas_returning$name != "Flory Bidunga"]) +
    sum(kansas_freshmen$nir_sal)
  )
)

message("Scenario A — Committed: $",
        format(budget_params$committed_a, big.mark = ","),
        " | Portal budget: $",
        format(budget_params$portal_budget_a, big.mark = ","))
message("Scenario B — Committed: $",
        format(budget_params$committed_b, big.mark = ","),
        " | Portal budget: $",
        format(budget_params$portal_budget_b, big.mark = ","))
