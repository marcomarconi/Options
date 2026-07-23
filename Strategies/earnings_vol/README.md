# earnings_vol

Earnings event-vol studies on the top-200 liquid single names, motivated by
Bennett *Trading Volatility* §6.4 (implied jump) — see the
`trading-volatility-book-bennett` memory.

**Strategy under test (Marco, 2026-07-10): LONG pre-earnings ATM straddle** —
buy the ATM straddle on the first expiry after the report ~15 calendar days
before the event, harvest the IV run-up, exit at the last close strictly before
the announcement (never holds the jump). Entry-lag grid 20/15/10/5 days.
The companion SHORT overnight-jump strategy (sell straddle at pre-event close,
cover next open) is NOT tested yet — smv EOD marks can't price the next-open
cover; needs intraday/open quotes.

## Data
- **Events:** reconstructed from raw ORATS core day files (`lastErn` +
  `lastErnTod`), since `nextErn` is never populated historically. 7,014 events,
  191 tickers, 2012-2024. `lastErnTod` is a CODE: 2=BMO, 3=AMC (verified
  JPM/WMT/KO/GS=2, AAPL/MSFT/NFLX/AMZN=3), else UNK.
  Point-in-time caveat: entering N days before a reconstructed date assumes the
  schedule was known in advance (true in practice; small lookahead risk on
  moved/unscheduled reports).
- **Point-in-time signal panel:** daily `impErnMv` / `impliedEarningsMove` /
  `absAvgErnMv` per name (for rich/cheap gating at entry).
- **Prices:** real NBBO straddle legs from
  `../ff_calendar_v2/cache/smv_single/` (±40% band, DTE≤70, 2013-2024); the wide
  retest reprices from `../_data/strikes/` (full universe, 2013-2024).

### Reproducing / data paths
The input data is **proprietary ORATS** and is NOT in this repo. Scripts reference it
by hardcoded local paths — edit these to your own locations before running:

| what | default path (edit in code) |
|---|---|
| ORATS core day-files (`orats_core_*.csv.gz`) | `~/trading/HistoricalData/ORATS/core` |
| per-strike NBBO archive (wide retest) | `~/trading/Systems/Options/Strategies/_data/strikes` |
| top-200 NBBO cache (baseline) | `~/trading/Systems/Options/Strategies/ff_calendar_v2/cache/smv_single` |

`cache/` (the built parquet panels, ~150 MB) is **git-ignored and regenerable** — rebuild
it locally, in order:

```bash
python3 pipeline/build_events.py        # baseline top-200 event calendar + panel
python3 pipeline/backtest_prern.py      # baseline trades  -> cache/trades_prern.parquet
python3 pipeline/build_events_wide.py   # wide-universe events (from _data/strikes tickers)
python3 pipeline/backtest_prern_wide.py # wide trades      -> cache/trades_prern_wide.parquet
```

Then any `analysis/*.py` (run from `analysis/`) reproduces its table/figure into `figures/`.
Requires Python 3.12 with `pandas`, `numpy`, `scipy`, `matplotlib`, `pyarrow`.

## Live screener (2026-07-15)
`python3 screener.py --capital 20000` — daily, run on a US trading day BEFORE
the close; prints today's LAG-2 entries (all confirmed events, no filters),
LAG-15 model-book entries (walk-forward OLS refit on the full 2013-2024
backtest, `--refit` to refresh; betas in `cache/live_model.json`), today's
EXITS, and a 5-trading-day watchlist. Sizing: `--capital <$$> [--pct <ann
std %, default 20>]` solves the equal-premium unit from the lag-2 backtest
(same bisection as analysis/unit_for_risk.py — trades/day, clustering and
integer rounding all come from the backtest, no extra inputs) and prints the
solved unit; `--unit <dollars>` sets it directly and overrides. Each BUY
line appends `-> Nx = $X premium` with N = unit // (100*mid); 0 contracts
= skip.
Signals log to `out/live_signals.csv` (merge-dedup on run/book/action/ticker,
same-day reruns keep the latest quote). A RESTRIKE CHECK section monitors
open lag-15 positions from the signal log: live net straddle delta
(σ√T backed out of the ATM straddle mid), trigger |Δ| ≥ 0.30 → prints
close-K/open-K' with bank-excess contract counts and logs a RESTRIKE row
(so the next run tracks the new strike). Lag-2 positions are never
restruck. Positions come from the signal log, not fills — ignore names
you didn't take.
Plumbing facts (verified live 2026-07-15): ORATS LIVE cores never populate
`nextErn` on this subscription (0/162 names) — but give `wksNextErn`
(weeks-out estimate) and `nextErnTod` as a CLOCK time (900=BMO, 1630=AMC;
historical files carry a CODE 2/3 instead). Exact dates come from yfinance
(single date = confirmed; unconfirmed dates are excluded from lag-2
entries), cross-checked against `wksNextErn`. Signal history (per-event
implied/realized) extends past the 2024 caches from the local core
downloads via `cache/ern_panel_live.parquet` (incremental; keep
`orats_downloader.py` runs current). Straddle quotes (expiry spanning the
jump, ATM strike) from ORATS LIVE strikes, fetched only for today's
entries. Trading calendar = core-file dates + hardcoded NYSE holidays
(extend the list yearly). Event-history construction verified bit-identical
to `analysis/oq_signals.build_event_table` on the 2013-2024 panel.

## Layout
- `screener.py` → daily live entry/exit report (see above)
- `pipeline/refresh_universe.py` → `out/top200_earnings_live.csv` + panel
  backfill for new names (see UNIVERSE REFRESH note; rerun quarterly-ish)
- `pipeline/build_events.py` → `cache/ern_panel.parquet`, `cache/events.parquet`
  (~5 min full scan; reuses cached panel if present)
- `pipeline/backtest_prern.py` → `cache/trades_prern.parquet`
  (filters: spot ≥ $5, debit ≥ $0.30, ATM within 10% of spot; marks MID = gross,
  ask-in/bid-out kept for the cost footnote)
- `pipeline/backtest_prern_lagscan.py` → `cache/trades_prern_lagscan.parquet`
  (same logic, fine lag grid 2..30 step 2, 94,286 trades)
- `analysis/gate.py` (past-only impErnMv rank — the honest gate),
  `analysis/audit_prern.py` (2026-07-11 audit: invariants, outliers,
  clustering, gate lookahead, marginal-pair dilution)
- `analysis/prern_analysis.py` (tables), `analysis/prern_plots.py` (figs 1-6),
  `analysis/prern_lagscan_plot.py` (fig 7 entry-distance profile),
  `analysis/volume_check.py` (fig 8), `analysis/sharpe.py` (monthly-series
  Sharpe by book), `analysis/prern_cum_lags.py` (fig 9 equity curves by lag),
  `analysis/oq_signals.py` + `analysis/oq_signals_plot.py` (fig 10 signal
  quintiles, fig 11b legacy s4-only gate), `analysis/oq_model.py` (fig 11
  video-faithful 4-signal walk-forward model), `analysis/price_split.py`
  (fig 12 both books by entry stock price), `analysis/combined_equity.py`
  (fig 13 core + sleeve combined equity; both use the model book),
  `analysis/weekly_split.py` (fig 14 weekly-listed vs monthly-only names;
  builds cache/weekly_flags.parquet from the smv strike cache),
  `analysis/lag2_signals.py` (fig 15 lag-2 entry-signal screen of 26 ORATS
  core fields + logVRP deep dive/tilt),
  `analysis/pnl_attribution.py` (fig 16 gamma/vega/theta PnL decomposition,
  both books; BS repricing of each straddle at entry/exit IV — exact, no
  residual), `analysis/sigma_r_mc.py` (ex-ante Monte-Carlo of σ_r from
  entry-observable inputs, no backtest — see Sizing section),
  `analysis/restrike.py` (mid-hold re-centering test, lag 15 + lag 2, θ
  grid 5-15% — see RESTRIKE RULE verdict), `analysis/restrike_delta.py`
  (vol-normalized net-delta trigger grid — the winner, see DELTA TRIGGER
  verdict),
  `analysis/unit_for_risk.py` (solve --unit for a target annual $ risk:
  `python3 unit_for_risk.py --capital 20000 --pct 20` → unit 825; bisection
  on the integer-contract realized ann-std, warns when peak-week premium
  deployment exceeds capital),
  `analysis/sizing_plots.py` (fig 17: equal-premium equalizes per-trade
  risk, ann-std vs unit curve, yearly $ P&L at unit 800, MC vs backtest
  σ_r), `analysis/restrike_plots.py` (fig 18: restrike vs hold cum-pnl,
  trigger rate by vol quintile, monthly Sharpe by rule),
  `analysis/amc_bmo.py` (fig 19: AMC vs BMO split, both books — see
  AMC vs BMO verdict), `analysis/crash_test.py` (fig 20: crash test vs
  passive core + weekend short-vol sleeve — see CRASH TEST verdict),
  `analysis/macropod.py` (out-of-sample replication on the independent
  RobotWealth macropod feather; writes `cache/macropod_trades.parquet`),
  `analysis/macropod_amc_bmo.py` (fig 21: AMC vs BMO there),
  `analysis/macropod_plots.py` (fig 22: eras, full history, cross-vendor
  per-trade check — see EXTERNAL REPLICATION verdict)
  → `out/`

## Verdict (audited 2026-07-11 — GROSS at mid only; costs ignored per house
## rule 2026-07-11, Marco doesn't trust EOD NBBO cost estimates)
**The long pre-earnings straddle is REAL gross.** 25,304 trades (191 names,
2013-2024), one per event x lag. The 2026-07-11 audit
(`analysis/audit_prern.py`) found the first-pass GATE numbers had lookahead
(impErnMv ranked vs FULL-sample own history); all gate figures below are the
corrected PAST-ONLY expanding rank (>=4 prior events, `analysis/gate.py`),
~25% smaller than the first-pass ones. Construction itself audited clean:
no dupes, date/expiry/filter invariants hold, edge survives outlier trimming
(clip +/-10%: +0.18% t=4.8) and event-month clustering (cluster t=4.1 vs
pooled 6.5).

- by lag: all positive; lag 15 best +0.309%/spot t=6.5 (lag 5 +0.23 / 10 +0.24 /
  20 +0.26). Win only ~41%, median −0.35% → positively skewed, mean carried by
  movers: quiet names (|spot mv| ≤ 3.7%) bleed −1.21%/spot, >p90 movers +6.98%.
  So the pnl is pre-event GAMMA (realized moves beat what the straddle prices
  in), not a pure "IV ramp mark-up"; unconditional edge is what's tradeable.
- by year: positive 10/12, worst year −0.05% (2014); strong 2018/22/24. Not
  one-regime.
- AMC >> BMO: +0.495 t=6.7 vs +0.121 t=1.9 (AMC exit rides the report-day
  final ramp; BMO gives that day up).
- rich/cheap gate (impErnMv PAST-ONLY rank vs own prior events): really a
  RICH-AVOIDANCE filter — cheap 1/3 +0.442 t=5.1, mid +0.357 t=4.4, rich 1/3
  +0.039 (dead); quintiles Q1-Q3 all +0.38..0.51, Q4 fades, Q5 zero.
  Bennett §6.4 survives as "don't buy when the move is priced rich".
- **best combo: AMC + cheap 1/3, lag 15: +0.566%/spot t=4.3 win 47%;
  +0.63% t=3.1 in 2019+.** Median debit 6.9% of spot, median hold 15 days.
- profitability by option volume (analysis/volume_check.py, fig 8): NOT an
  illiquidity artifact — flat-to-RISING in liquidity (top $-volume quintile
  best +0.39 t=3.5; same in contracts/OI); within-ticker, unusually busy
  option days are the best trades (+0.56 t=4.5 vs ~+0.22) — observation only,
  not stacked into the gate; gate works in both volume halves (+0.63/+0.49).
- entry-distance scan (lag 2..30 step 2, fixed pre-event exit): ungated mean
  is FLAT +0.20..0.33%/spot across the whole range, but t rises as lag shrinks
  (lag 2: +0.22% t=12.0 over a ~2-day hold) → pnl per day / per unit risk is
  concentrated in the FINAL days; adjacent-lag marginals (same event, distinct
  entries) show no clean accrual structure — far window adds variance, not
  mean. Gated line sits above ungated everywhere but is noisy (~800/point);
  no reliable preference for long entries.
- Oakquants 4-signal test (2026-07-11, analysis/oq_signals.py; video claims
  4 relative-value predictors): CONFIRMED on lag 15 — all four monotone with
  return, ICs -0.04..-0.09; implied-vs-REALIZED signals (s2 vs last realized,
  s4 = impErnMv - expanding avg |realized jump|) are the genuinely NEW info
  (spearman vs our mv_rk only 0.14/0.66; s3 vs-avg-implied = our gate,
  rho 0.84). Double-sorts: each adds on top of the other. Simple upgraded
  gate (AMC + s4-low-half, fig 11b): beats the old AMC+mv_rk-tercile on
  every axis — n=1207 vs 817, +0.60%/spot t=5.7 vs +0.57 t=4.3, monthly
  Sharpe 1.36 vs 1.05, 2019+ +0.65%. At LAG 2 the signals are weak/U-shaped
  (even rich-implied trades pay in the final days) → lag-2 book stays
  ungated. Event-table facts: median event implied 7.4% vs realized 3.6%
  (imp/rlz ~2x — why holding through the jump is a bad long).
- **VIDEO-FAITHFUL 4-SIGNAL MODEL (2026-07-12, analysis/oq_model.py,
  fig 11): walk-forward OLS on all four signals (expanding window, retrained
  yearly, first test year 2016, features winsorized at train 1/99 pct),
  trade pred>0, all sessions — OUT-OF-SAMPLE keeps 75% of trades (n=3403),
  lifts +0.28→+0.38%/spot t=6.3, monthly Sharpe 0.91→1.26 on the same 2016+
  window; AMC-only subset +0.55% Sharpe 1.37 ≈ the simple s4 gate (the
  regression mostly re-picks the same trades — consistent with the VRP-model
  lesson that regression adds little over simple sorts). Prediction-decile
  calibration rising but noisy (D1 negative, D9-D10 +0.6-0.85%).**
- OQ-WORK AUDIT (2026-07-12, analysis/audit_oq.py) PASS: event table 0 dupes,
  merge preserves rows, 3.9% of ernDates fall on non-trading days (handled:
  reaction = next trading day); signals verified past-only by brute-force
  recomputation (0 mismatches AAPL/NVDA/WMT); model robust to refit-boundary
  (strict same-year subset +0.36% t=5.9) and to feature transform
  (rank-features +0.44% t=6.6 Sharpe 1.45 — even better, not adopted to
  avoid post-hoc shopping); walk-forward s4-only top-75% keep rule ties the
  model (Sharpe 1.27 vs 1.26, 81% trade overlap) = regression ≈ s4 CONFIRMED
  out-of-sample; betas stable in sign (s3 consistently negative, s1 flips on
  s1/s3 collinearity). Sharpe-convention note: s4-gate 1.36 was measured
  from its first trade month; on the standard full 2013-2024 window it is
  1.30 (same ordering everywhere; window stated per figure).
- **Sharpe (monthly-series convention: equal notional, pnl at exit, empty
  months = 0, x sqrt(12)) is monotone in lag: lag 2 = 2.0, lag 4 = 1.6,
  lag 8 = 1.35, lag 14 = 1.0, lag 22 = 0.8, lag 30 = 0.5.** Per-trade std
  scales with hold (1.4% at lag 2 vs 5.6% at lag 30) while mean is flat, and
  short holds recycle capital ~7x faster. LAG-2 BOOK is the recommended
  trade (2026-07-11): positive ALL 12 years (worst 2016 +0.09), win ~53%,
  both sessions positive (AMC +0.30 t=10.4, BMO +0.12 t=5.9), rich tercile
  still positive → FILTERS DON'T PAY at lag 2 (ungated Sharpe 2.02 > AMC-only
  1.69 > AMC+not-rich 1.72; breadth beats selection). Lag-15 gated book =
  capital-light alternative, Sharpe ~1.05. Live gap: needs forward earnings
  calendar (live ORATS API populates nextErn; historical files don't).

- by entry stock price (analysis/price_split.py, fig 12; sleeve = 4-signal
  model book since 2026-07-12): lag-2 edge FALLS with price — sub-$40 names
  +0.34..0.45%/spot, $40-300 +0.13..0.16% (t=4-5), >$300 weak (+0.09 t=1.7);
  restricted to spot>=$40 the book is +0.136% t=8.4, monthly Sharpe 1.48
  (vs 2.02 all). The 4-signal model book is much flatter in price
  (+0.26..0.46% from $5 to $300, fades only >$300) — no small-stock tilt
  (same held for the earlier s4-only gate). Hybrid test with the s4 gate
  (lag-2 on <$40 + gated-15 on >=$40): Sharpe 2.16 vs lag-2-all 2.01 —
  within noise (SE ~0.3), post-hoc partition, NOT adopted.
- LAG-2 ENTRY-SIGNAL SCREEN (2026-07-12, analysis/lag2_signals.py, fig 15):
  26 ORATS core fields at entry close (bar: |IC|>=0.04 + same sign both
  halves + within-ticker IC). **FIRST-PASS WINNER logVRP RETRACTED — it is
  FORWARD-LOOKING**: Options.R computes logVRP = log(iv30d /
  lead(orHv20d, 20)) = IV vs the NEXT 20 days' realized vol, a window
  containing our hold and the earnings jump (caught by Marco's "check for
  lookahead" request; sharpe_two's builder had already flagged
  logVRP/straPro*/straRet* as forward). Honest trailing version
  log(iv30d/orHv20d): IC -0.001 = DEAD (ex-earn variant -0.016). Extract
  semantics: pxAtmIv = STOCK PRICE, retAtmIv = clipped daily stock return.
  CLEAN survivors: stkPxChng1m -0.077 (Q1 losers +0.48% t=7.1, rest flat),
  stkPxChng6m -0.074, correlSpy1y -0.066, retAtmIv +0.056 — real per-trade
  info for prioritization, but every tilt/gate on them LOWERS monthly
  Sharpe (1.85-1.93 vs 2.01). VERDICT: lag-2 stays UNGATED and UNWEIGHTED;
  the screen hardened that conclusion rather than refining the book.
- weekly vs monthly-only listings (analysis/weekly_split.py, fig 14;
  classified per trade at entry from the strike cache, non-3rd-Friday expiry
  1-40d out = weekly): universe is 94% weekly names (98%+ post-2015) — both
  books are effectively weekly-name strategies. Monthly-only sliver at lag 2
  is FORCED into a ~14-17-DTE straddle (vs 4-DTE median) at ~2x the premium
  yet earned MORE (+0.50% t=3.6 full sample; +1.14% n=98 on 2016+) — edge
  not dependent on weekly listings, possibly less competed without them; too
  small to use as a filter. Model book monthly-only n=31 (-0.47% t=-1.0):
  no conclusion possible.
- combined book (analysis/combined_equity.py, fig 13; common 2016+ OOS
  window): lag-2 Sharpe 1.92, 4-signal model book 1.26, monthly corr +0.39;
  at equal per-trade notional the combined book earns ~2.2x the pnl but
  Sharpe dilutes to 1.76 (model book = 75% of events on 15-day holds); at
  half/quarter sleeve weight Sharpe 1.92/1.99 → the sleeve adds pnl
  CAPACITY, not risk-adjusted performance. (Earlier s4-gate version of this
  test — small sleeve, n=1207 — was Sharpe-neutral at full weight: 2.05 vs
  2.01 on 2013+.)
- RESTRIKE RULE (2026-07-17, analysis/restrike.py → cache/
  restrike_results.parquet, fig 18): when spot drifts ≥θ from the current strike
  mid-hold, close at mid and reopen the nearest ATM straddle on the SAME
  expiry (repeatable; "bank-excess" sizing = new premium capped at the
  original unit, profits banked). **Lag-15: RESTRIKE AT θ=5% IS A REAL
  UPGRADE** — 53% of trades trigger (avg 1.79 restrikes); all-trades mean
  +4.58→+6.13% of debit, per-trade std 42.6→35.9%, t 8.5→13.6, monthly
  Sharpe (equal-premium conv.) 1.23→1.78. Mechanism confirmed by the
  close-early arm: after a trigger the ORIGINAL ITM straddle's continuation
  is zero-mean noise (close-early ≈ hold mean at half the variance), while
  the re-centered straddle keeps earning (triggered subset +20.6% vs +17.6%
  hold, std 39.7 vs 51.5, win 71% vs 56%) — the delta ride is not where the
  edge is, fresh gamma near the event is. Monotone: tighter θ better down
  to 5% (grid edge; below that strike granularity binds); bank-excess
  dominates reinvest-all risk-adjusted (reinvest = more mean, much more
  variance). Restrike cuts lag-15 σ_r 0.459→0.359 (sizing input). **Lag-2:
  irrelevant** — only 4% trigger at θ=5% in the ~2-day hold, moSharpe
  2.54→2.63 ≈ noise; not worth the operational load. Caveats: extra
  executions marked at MID per house rule (ITM straddles have wider
  spreads); rule is mechanical, not fitted (4-point monotone grid).
- **DELTA TRIGGER BEATS THE %-MOVE TRIGGER (2026-07-17,
  analysis/restrike_delta.py → cache/restrike_delta_results.parquet,
  fig 18)**:
  trigger on straddle NET DELTA |2Φ(d1)−1| ≥ D, with remaining σ√T backed
  out of the current ATM straddle mid (σ√T ≈ atm_mid/(0.8·spot), no IV
  solver). Lag-15, bank-excess sizing: D=0.20 → moSharpe 2.04 (mean +6.77%
  of debit, σ_r 0.312, 95% trigger, avg 3.0 restrikes); D=0.30 → 1.91
  (+6.54%, 84%, 2.0 restrikes); D=0.40 → 1.82; D=0.50 → 1.75 ≈ the 5%-move
  rule (1.78). Monotone tighter=better to the grid edge 0.20 (untested
  below; near-daily churn). WHY it wins: the %-rule's trigger rate ramps
  15%→90% across entry-vol quintiles (5% is 2σ for a staple, noise for a
  biotech); the delta rule is FLAT ~85% across all quintiles — it
  normalizes for vol and shrinking T, spending restrikes where the
  position actually degenerated into delta. OPERATIVE RULE (lag-15 only):
  restrike when |net delta| ≥ 0.3 (practical churn/benefit balance; 0.2 if
  executions are cheap), bank-excess sizing, never on the exit day. With
  this rule the lag-15 book's equal-premium moSharpe ~1.9-2.0 vs lag-2's
  2.54 — gap narrowed, allocation conclusion (lag-2 first) unchanged, but
  a restruck-lag-15 + lag-2 blend is now worth a fresh look (corr +0.39
  measured pre-restrike).
- AMC vs BMO (2026-07-19, analysis/amc_bmo.py, fig 19): after-close
  reporters carry most of the per-trade edge in every book. Lag-2 ungated:
  AMC +0.304%/spot t=10.3 vs BMO +0.121% t=5.9 — but on MONTHLY Sharpe the
  gap nearly closes (1.69 vs 1.57), so BMO trades still pull their weight
  in the core; keep them (breadth beats selection at lag 2, again). Lag-15
  ungated: AMC +0.478% t=6.5 vs BMO +0.126% t=1.9 (moSharpe 1.14 vs 0.34)
  — BMO is barely distinguishable from zero over a 15-day hold. With the
  past-only s4-low gate (expanding median, no AMC condition in the gate):
  AMC +0.640% t=5.3 vs BMO +0.293% t=3.1 (moSharpe 1.22 vs 0.66) — the
  gate helps both sides but the AMC/BMO gap stays ~2x, which is why the
  recommended lag-15 model book is AMC-only. Mechanism (hypothesis,
  consistent with the gamma attribution): AMC trades exit at the close of
  the announcement day itself — hours before the report, capturing that
  final day's drift/positioning and the steepest part of the IV ramp —
  while BMO trades must exit at the prior close, one full trading day
  further from the event.
- **CRASH TEST — the true diversifier (2026-07-19, analysis/crash_test.py,
  fig 20)**: lag-2 book (fee floor $0.75, equal premium, vol-matched to
  10%/yr like the complement-study sleeves; common window 2014-06..2024-12)
  vs the passive world-equity core and the EMA-filtered weekend short-vol
  sleeve. Lag-2: Sharpe 2.64 (gross at mid), corr to core **−0.02**,
  down-month corr −0.08, maxDD −9%. POSITIVE IN ALL FOUR named crash
  windows: 2015 China deval +22.6%, 2018 Q4 +10.8%, 2020 COVID +12.8%,
  **2022 bear +48.7%** (its best stretch — big earnings moves, rich gamma).
  Worst-decile core months: avg +2.97%/mo, negative in only 8% of them
  (weekend sleeve: +1.12%, 23%). Character: NOT a convex crash hedge (no
  negative beta spike) — a zero-beta carry stream whose earnings-season
  engine keeps paying through stress. Blends (100k core + 30k sleeve):
  core alone 0.66 / +weekend 0.95 / **+prern 1.20** / +50-50 mix 1.08;
  maxDD −20/−15/−13/−14%; COVID window −15.5% → −12.8%. This ANSWERS the
  [[crash-conditional complement]] question better than the weekend sleeve
  did — with the caveats that prern Sharpe is gross at mid (per-contract
  fees ≈ halve it standalone), 2021 Q1 gap months sit at 0, and the two
  sleeves can be RUN TOGETHER (corr weekend↔prern −0.03 full sample, +0.20
  in core-down months) — the mix is the practical choice, not either/or.
- **EXTERNAL REPLICATION — independent vendor, +7 unseen years (2026-07-20,
  analysis/macropod.py + macropod_plots.py, fig 22)**: rebuilt all three
  books on the RobotWealth *macropod* feather (`RobotWealth/macro-pod/
  research/options/straddle_over_earnings/macropod/earnings_straddles.
  feather`) — a separate ORATS pull, 4108 tickers, 2006 to **Feb-2024** (the file
  ends 2024-02-16), real NBBO on both legs, 128k events. Same trade logic as `backtest_prern.py`.
  **Everything replicates, and the ordering of the books is preserved**
  (top-200 by volume, 2013-2024): lag-2 ungated +0.185%/spot t=7.2 moSh
  1.39 (ours +0.31% t=6.5); lag-14 naive +0.128% t=2.3; lag-14 + s4-low
  gate **+0.409% t=6.5 moSh 1.50** (ours +0.60% t=5.7); lag-14 + WF OLS
  +0.312% t=5.0 — i.e. the gate still ~doubles naive, and the walk-forward
  OLS still lands NEAR the simple s4 gate without beating it, exactly as on
  our own data. **2006-2012 is genuinely new**: lag-2 holds (+0.257% t=19.1
  all names; +0.126% t=4.2 top-200) but naive lag-14 is flat-to-negative
  top-200 (−0.04%, t=−0.8) and the gated books are weaker (+0.19% t=2.5) —
  the long-dated run-up looks era-dependent, the SHORT lag-2 window is what
  persists across both eras. Strongest single piece of evidence: on the
  **3823 lag-2 events present in BOTH datasets**, per-trade returns correlate
  **r=0.75 (AMC)** / 0.47 (BMO) with means agreeing to 1-2bp (AMC ours
  +0.307% vs mp +0.292%; BMO +0.062% vs +0.065%) — two vendors, same trades,
  same answer. LIMITS: (a) macropod carries ONE fixed strike per event, ATM
  as of ~14d out, so the lag-2 book cannot re-strike at entry as ours does
  (the 10% ATM filter keeps it honest but selects mildly for low-drift
  names); (b) `extSmvVol` is unusable there (30-45% of rows imply a zero
  earnings jump, median ~0.5% vs a true ~5%), so ORATS `impErnMv` cannot be
  rebuilt — s1-s4 use a straddle-premium PROXY, making B/B' corroboration
  rather than like-for-like; (c) lag **14** not 15, because 78% of macropod
  panels start at −14 (asking for 15 silently discards 3/4 of events);
  (d) raw file needs a debit/spot ≤ 0.40 hygiene cap (it contains $10k
  straddles on $73 stocks — our own book's max is 0.386, so nothing real is
  cut; without it the all-name lag-2 book reads −0.10% t=−0.5 on n=65k off a
  single bad print). DATA LESSON: macropod's `daysToCover` is anchored on
  the EXIT day (last close before the jump), NOT the earnings date — for a
  BMO report `dtc=0` is the day BEFORE it. Treating it as earnings-relative
  sells every BMO trade a day early and kills the BMO edge entirely (see
  fig 21 note).
- **AMC vs BMO, EXTERNAL CHECK (2026-07-20, analysis/macropod_amc_bmo.py,
  fig 21)**: confirms fig 19 on independent data — AMC dominates BMO in
  EVERY cut (both universes, both eras, all three books). Top-200
  2013-2024: lag-2 AMC +0.327% t=6.8 vs BMO +0.064% t=2.8 (ours: +0.304%
  t=10.3 / +0.121% t=5.9); s4-gated AMC +0.567% t=6.4 vs BMO +0.126% t=1.8
  (ours +0.640% / +0.293%). The **AMC column replicates almost exactly**;
  BMO is consistently ~half our numbers and turns NEGATIVE at lag-14 in
  2006-2012 top-200 (−0.179%, t=−2.5). Read: the AMC tilt is real and if
  anything stronger than our own data says; BMO is not dead (positive at
  lag-2 in 3 of 4 cuts) but needs the FULL run-up window to be worth
  anything, and is the first thing to cut if capital or trade count must be
  rationed. BONUS VALIDATION: our inferred `when` (ORATS `lastErnTod` code
  2=BMO/3=AMC) agrees with macropod's explicit `earningsTime` on **99.2%**
  of 3317 matched events — that mapping had never been checked against an
  independent source, and it is now confirmed.
- PNL ATTRIBUTION — gamma vs vega (2026-07-15, analysis/pnl_attribution.py,
  fig 16): BS-reprice each straddle at entry/exit implied vol (backed out
  from the observed mids, r=q=0); split mid-to-mid PnL exactly into theta
  (time, fixed spot & IV) + gamma (spot move at entry IV) + vega (IV
  re-mark); order-averaged, cross-term tiny. Raw greeks per mean trade:
  lag-2 gamma +0.47 / vega +1.53 / theta -1.79 → +0.21%/spot;
  model book gamma +2.45 / vega +2.56 / theta -4.72 → +0.29%/spot. IV rises
  into the event in 96-99% of trades (lag-2 median 62%→83%; model 37%→82%)
  — but that ramp is mostly MECHANICAL (a constant event premium spread over
  fewer remaining days = higher annualized IV), so the economic split is
  gamma vs FIXED-SPOT re-mark (theta+vega): lag-2 -0.26%/spot, model
  -2.16%/spot — the straddle held at a frozen spot LOSES money in both
  books; the vega ramp never fully pays the decay. **VERDICT: both books
  are GAMMA trades** — all net profit comes from the underlying moving more
  than the (theta-net-of-IV-ramp) bleed, consistent with fig 3 (quiet names
  bleed, movers pay) and the Sharpe-vs-lag monotonicity (shorter hold =
  less fixed-spot bleed per unit of pre-event realized move). Component
  stability: gamma and vega components positive every exit year, both
  books.

## Sizing & risk management (2026-07-17) — fig 17
**EQUAL PREMIUM is the correct per-trade weight**: spend the same dollar
debit on every trade, contracts = unit // (100*mid). The straddle price
already scales with both stock price and vol (debit/spot ~ 0.8*sigma*sqrt(T)),
so dividing by the debit normalizes both at once — "vol-scaled notional"
(pnl/spot ÷ debit/spot) is mathematically IDENTICAL to pnl/debit. Verified
on the backtest: across debit/spot quintiles (2.8%..12.8% of spot) per-trade
std of pnl/SPOT ramps 5x (0.54%→2.67%) while pnl/DEBIT is flat (~16-25%);
same on lags 5-20 pooled (0.37-0.41 all quintiles vs a 4.4x ramp). Bonuses:
per-trade Sharpe is HIGHER under equal premium (lag-2 0.186 vs 0.153; pooled
0.106 vs 0.076 — equal notional underweights cheap-vol names that carry
equal edge), and the loss tail is capped by construction at −1 unit (worst
observed −75% of debit ≈ −2σ, vs −6..−9σ outliers under equal notional).

**Risk formula (theory)**: annual $ std = unit · σ_r · √N ·
√(1+(k−1)ρ) · deployment-ratio, where σ_r = per-trade std of pnl/debit
(MEASURED as std(ret_debit_mid) over the backtest trades: 0.189 on the
6,146 lag-2 trades, 0.459 on the 3,403 model-book trades; ungated lags
5-20 pool ~0.38. Theory bounds it ≤1 (max loss = debit) and predicts the
~√hold scaling between books: √(14d/2d)=2.65 vs observed 0.459/0.189=2.4.
NO-BACKTEST estimate (analysis/sigma_r_mc.py): Monte-Carlo a forward model
from entry-observable inputs — total implied straddle variance = implied
jump² (impErnMv) + daily-HV²·DTE, hold = fat-tailed diffusion (t4) +
re-pricing of the jump (rel. vol ~0.10), event variance carries (its IV
ramp is mechanical) — gives σ_r = 0.187 vs 0.189 measured, p5 matches;
the two assumptions dominate: Gaussian tails + frozen jump → 0.11,
so tails/vol-of-jump set the ±30% precision of any ex-ante unit), N = trades/yr, k = avg overlapping positions, ρ = their
pairwise correlation. The iid version (drop the √(1+(k−1)ρ) term) is a
FLOOR on risk: empirically earnings-week clustering multiplies annual std
by ~1.66 (lag-2: 7.1 units of ann-std per unit, monthly conv., vs 4.3 iid)
to ~2.3x (both books: 21 vs 9.4). Integer contracts cut realized risk back
(at $500/trade only 69% of signals get ≥1 contract, avg spend 82% of unit
→ realized std ~65% of fractional ideal; at $1000: 85%/86%).

**Empirical per-unit numbers (daily book P&L landed on exit date)**: lag-2
ann-std/unit 5.7 (daily conv.) / 7.1 (monthly) / 8.1 (yearly-sum), mean
+18.1 units/yr, maxDD −5.6 units, never a losing year. Both books 2016+:
ann-std 21-27, mean +39/yr, maxDD −24.7, worst year +0.0. **At a fixed risk
budget run LAG-2 ONLY** (return/risk ~2.5 vs ~1.9 combined — the model
sleeve adds capacity, not Sharpe, corr +0.39 per fig 13); the sleeve earns
its place only when the account outgrows the lag-2 book.

**Worked case ($20k account, 20% ann-std target = $4k)**: `--unit 800`
→ realized ann std $3,880 with whole contracts (naive iid says $930,
clustering pushes to ~$560, rounding back up to ~$800 — the corrections
partially cancel). Historical (2013-2024, gross at mid): mean +$11.9k/yr,
ALL 12 years positive (worst 2021 +$4.4k, and 2021 is missing Q1), maxDD
−$2,780 (−14%); std is mostly UPSIDE dispersion (positively skewed book).
Binding constraint is CASH not risk: premium deployed median $1.1k,
p90 $5.6k, but peak week $18.5k ≈ the whole account → occasionally skip
late signals in peak earnings weeks. Granularity: at $800-1000 units,
straddles > $8-10 mid round to 0-1 contracts (big-caps run $15-30) — the
screener prints the skip explicitly.

**Re-sizing when capital or risk %% changes**: run
`analysis/unit_for_risk.py --capital X --pct Y` (or `--risk $`). Pocket
rule: unit ≈ target-$-std / 5, but the true ratio drifts with unit size
(4.2 at $500 → 5.6 at $1500, integer rounding fills better at bigger
units) — use the solver, it's exact on the backtest and warns when the
peak-week cash need exceeds the account. Lag-15 model book (if ever run
for capacity): ann-std ≈ 9.3 $/unit-$ before rounding, unit ≈ its
risk allocation / 9-10; standalone at $4k std → unit ≈ $420, but
return/risk ~0.9 vs lag-2's ~3.1, losing years, 50% granularity → not
recommended below ~$60k accounts.

**COMMISSIONS & MIN-DEBIT FLOOR (2026-07-19, Marco's actual fees — this is
his known fixed cost, distinct from the gross-at-mid spread convention)**:
Marco pays PER CONTRACT, ≈$4 per contract round-trip ($1/contract/leg).
Equal-premium sizing buys MANY contracts of cheap straddles (14 avg at mid
< $0.75), so fee load is 4/(100·mid) of premium — 7.2% at the cheapest
band vs the +3.83% mean edge → cheap straddles are EV-negative net. At the
old $0.30 floor total fees are $7.1k/yr = 62% of the $11.4k gross (!);
monthly Sharpe drops ~2.8 gross → ~1.3 net. Floor grid (unit re-solved to
$4k ann-std at each): net P&L and net Sharpe both peak at **debit ≥ $0.75**
(net $5.0k/yr, Sharpe 1.43, fees $6.0k), flat plateau to $2.50, worst at
$0.30. $0.75 beats the naive $1.04 breakeven (= fee/mean-edge) because
cheap straddles carry above-average %-edge (see price split, fig 12).
ADOPTED: screener MIN_DEBIT 0.30 → 0.75 (live only — the backtest record
and its $0.30 sanity floor are unchanged); unit_for_risk.load() applies
the same floor, so the $20k/20% solve moves 825 → 925 (ann-std $4.0k,
gross +$11.0k/yr, ~$5.0k/yr net of fees). If fees were flat per ORDER
instead, no floor would be needed (min deployed premium ≈ unit/2 ≫ $104
breakeven) — revisit if the broker changes.

LAG-2 AUDIT (2026-07-11, analysis/audit_lag2.py) PASS: exit-day underlying
move ordinary (median 1.45% AMC) while the day AFTER exit carries the jump
(median 4.68%) → we exit at the last quiet close, no announcement leakage
(suspects 0.55% of trades / 6.3% of pnl; edge excl. +0.21% t=11.6); edge
survives bid-to-bid (+0.183 t=10.4) → not a spread-widening mid artifact.
DATA TRAP: ORATS core pxCls = PREVIOUS close (row t carries close(t-1));
smv stk is same-day. Unshifted pxCls returns fake an off-by-one calendar.

DATA GAP: the smv_single NBBO strike cache has NO quotes Jan-Apr 2021 (all
names; source /media/marco/Elements/ORATS/smvstrikes, drive not mounted to
check raw) → zero trades those 4 months in every book, the flat stretch in
the equity curves. One missed earnings season; 2021 stats are May-Dec only
(lag-2 2021: n=384 +0.26%). Only gap in the sample (other thin months are
the normal earnings-calendar rhythm).

Caveats: reconstructed calendar assumes the report date was known ~15d ahead
(true in practice, small lookahead risk on moved dates); gate tercile cutoffs
still chosen in-sample; overlapping-name event clustering inflates pooled t
(month-cluster t=4.1); universe = top-200 by full-sample liquidity (mild
survivorship).
UNIVERSE REFRESH (2026-07-19, pipeline/refresh_universe.py): the historical
top-200 list (ff_calendar_v2, ranked 2013-2024) had decayed — 38 tickers
dead (renames/acquisitions/bankruptcies: FB, TWTR, SQ, PCLN, ATVI, CELG...)
so the live screener was down to 162 names. The refresh script rebuilds a
current top-200 from the last ~126 local core day files: median daily option
contract volume (cVolu+pVolu), keeping only names that REPORT EARNINGS
(lastErn within 200d — this alone excludes all ETFs/indices/single-stock
ETFs), cover >= 90%, px >= $5. Writes out/top200_earnings_live.csv (the
historical csv is untouched — backtest record unchanged); screener.py now
prefers the live csv with fallback to the old one. First run (asof
2026-07-17): 103 names kept, 97 swapped in (META, XYZ, DELL, TSM, LLY, ARM,
MSTR, RDDT, SMCI...; out: the 38 dead plus names below today's ~17k
contracts/day cutoff — MA, MO, ABBV, MCD...). The script also backfills
cache/ern_panel_live.parquet with full 2013->today history for new tickers
(+174k rows, 264 tickers) so the s4/model signals have their expanding
past-only aggregates immediately. Screener additionally keeps any name with
an OPEN logged position even after it leaves the universe (exit lines come
from the live scan, not the log). Rerun quarterly-ish.

NEXT: walk-forward on the gate, per-name breadth, delta-hedged variant to
isolate the vega ramp, SHORT overnight-jump leg. (Portfolio sizing DONE
2026-07-17 — see Sizing section.)
