# Kansas Basketball — 2026–27 Transfer Portal Analysis

## 📋 [VIEW FULL REPORT →](REPORT.md)

The report covers both roster scenarios, all portal target rankings, undervalued
player deep-dives, and budget optimization. No code required — designed to be
read directly in the browser.

---

## Scenarios at a Glance

| | Scenario A | Scenario B |
|---|---|---|
| **Retention factor** | Bidunga stays | Bidunga leaves |
| **Portal budget** | ~$2.3M | ~$7.6M |
| **Top need** | Guards + wing | Center + guards + wing |
| **Projected KenPom** | Top 5–10 | Top 3–8 (w/ Broome) |

## Key Findings

- **Carter + Davis** appear on every optimized roster regardless of scenario — securing both should be the #1 recruiting priority
- **Johnell Davis (FAU)** is the top undervalued target: top-20 nationally in offensive efficiency, plays elite defense, likely priced $400–700K below a P4 player with equivalent numbers
- **Rubin Jones (Cincinnati)** is the undervalued center option in Scenario B — elite ORB% and block rate at ~40 cents on the dollar vs. comparable P4 bigs
- Both scenarios project as **Big 12 title contenders and realistic Final Four rosters**

---

## Analytics Files (for staff / data team)

| File | Description |
|---|---|
| [`player_data.R`](player_data.R) | Player database — returning roster, freshmen, full portal target pool |
| [`nil_market_model.R`](nil_market_model.R) | NIL salary inference model with methodology |
| [`roster_optimizer.R`](roster_optimizer.R) | Greedy roster optimizer — maximizes efficiency within budget + position constraints |
| [`kansas_portal_report_2026.Rmd`](kansas_portal_report_2026.Rmd) | R Markdown source for a rendered HTML version |
