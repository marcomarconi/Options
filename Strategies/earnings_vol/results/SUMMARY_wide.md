# Pre-earnings straddle — wide-universe retest on real quotes (2026-07-23)

The baseline study priced the LONG pre-earnings ATM straddle (buy ~LAG days before the
report, sell at the last close strictly before the announcement, never hold the jump) from
the ff_calendar_v2 **top-200** `smv_single` cache — 7,014 events, 191 names. Earnings studies
are event-count-bound, so this retest widens the universe to the full `_data/strikes` archive.

**115,580 events / 3,839 names** (build_events_wide.py, from raw core `lastErn`/`lastErnTod`),
priced from `_data/strikes` (backtest_prern_wide.py) → **351,438 trades, 3,659 names, 2013–2024**
— a **14× wider name universe, 16× more events** than the baseline. Trade logic, filters, and
marks are unchanged; only the price source and universe changed.

## Gross: the IV run-up is real and robust across the whole market
Mean return in % of spot (house convention), by entry lag:

| lag | n | gross % | t | win % |
|---|---|---|---|---|
| **5** | 88,350 | **+0.236** | **+24.6** | 46 |
| 10 | 87,936 | +0.218 | +17.3 | 43 |
| 15 | 87,675 | +0.185 | +12.5 | 41 |
| 20 | 87,477 | +0.060 | +3.6 | 39 |

Gross positive in **12 of 12 years** at lag 5 (min 2014 +0.03%, max 2020 +0.59%). The anomaly
survives the 14× expansion overwhelmingly (t=24.6). **Shape shift vs baseline:** the top-200
edge peaked at lag 15 (+0.31%); on the broad universe it is strongest at **lag 5** and decays
with lag — on less-liquid names the earlier-entry theta bleed eats more of the run-up, so the
tradeable version enters closer to the event.

## Net: a liquidity-U — clears only in a moderate-liquidity "Goldilocks" bucket
The straddle round-trip spread (cross bid/ask on both legs, entry and exit) scales steeply with
illiquidity and dwarfs the ~0.2–0.3%/spot edge almost everywhere. Lag 5, by `avgOptVolu20d`:

| avgOptVolu20d | n | gross % | t | realistic net (f=0.5) % | full taker % | round-trip spread %spot |
|---|---|---|---|---|---|---|
| <1k | 44,063 | +0.243 | 15.9 | −2.28 | −4.81 | 5.05 |
| 1–10k | 32,999 | +0.222 | 16.5 | −0.80 | −1.83 | 2.05 |
| 10–50k | 7,781 | +0.165 | 6.9 | −0.27 | −0.70 | 0.87 |
| **50–250k** | **1,693** | **+0.344** | **4.96** | **+0.14** | **−0.07** | **0.41** |
| 250k+ | 260 | +0.096 | 0.58 | −0.01 | −0.12 | 0.21 |

The pattern is a **U in liquidity**:
- **Illiquid (<10k):** fat gross t-stats but 2–5%/spot round-trip spreads → net −0.8% to −2.3%. Dead.
- **Ultra-liquid (250k+):** the tightest spreads (0.2%) but **the run-up is arbitraged away** —
  gross +0.096%, t=0.58, n=260 (the megacaps whose earnings vol everyone watches). No edge to net.
- **Moderate (50–250k):** the only bucket that clears — gross **+0.344%** (t=4.96) *beats* the
  0.41% round-trip spread, so realistic net is **+0.14%/spot** and even full-taker is only −0.07%.
  *(But this net edge is thin and NOT temporally robust — see Verdict: ~half the P&L is 5 months,
  negative through the calm years. Do not read the full-sample average as a steady edge.)*

## Verdict — the wide universe CONFIRMS the baseline; it does not change it
The pre-earnings IV run-up is a **genuine, highly significant gross anomaly** (t=24 across 88k
events, positive every year), and it is **execution-bound** — ~0.24%/spot, beaten by the
straddle's own spread in almost every liquidity tier. Both statements were already the top-200
verdict; the value of this run is that they **hold on 14× the name universe**, not that they are new.

Everything the wide cuts surfaced was already established on the top-200 sample and should not be
read as a fresh finding:
- **AMC ≫ BMO (~2× gross), AMC positive every year, mechanism = AMC rides the report-day close /
  BMO gives that day up, recommended book AMC-only** — all in the baseline (README §"AMC vs BMO",
  `analysis/amc_bmo.py`, externally cross-checked in `macropod_amc_bmo.py`). The wide data merely
  reproduces it. The baseline was in fact more careful (on *monthly Sharpe* the AMC/BMO gap
  narrows at lag 2 — "BMO trades still pull their weight").
- **The mid-liquidity net "niche" (50–250k, short lag, AMC) is real but weak and NOT robust.**
  Full-sample it clears (AMC 50–250k lag 5: gross +0.54%, realistic +0.32%, taker +0.09%), but a
  robustness check shows it is thin and regime-loaded: top-5 months ≈ 52% of the P&L, only ~50% of
  months positive, negative/flat through the calm years (2013–16, 2019–20), edge concentrated in
  high-vol seasons (2018, 2021–22, 2024). A lumpy Sharpe, not a steady stream. Do not oversell it.

**Bottom line:** wide universe = a robustness confirmation of the top-200 study (real gross
anomaly, execution-bound, AMC-dominated, thin regime-dependent net in mid-liquidity names). No
change to the live books or the screener is warranted.

Figure `figures/wide_earnings.png`. Trades `cache/trades_prern_wide.parquet`
(`pipeline/build_events_wide.py` → `pipeline/backtest_prern_wide.py` → `analysis/wide_report.py`).

## Still open (integrity only)
- `analysis/lag2_signals.py` reads the tainted `ORATS_core.pq` — repoint to raw core (23 of 26
  fields exist there; `retAtmIv` and the forward `logVRP*` do not, the latter already excluded).
  This is the one loose end; the screener and the live books are fine as-is.
