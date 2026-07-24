# vrp_etf_v2 — VRP short-straddle retest on permitted data

Retest of the Option-Quants / Oakquants YouTube **"VRP model"**: sell 30-day ATM straddles,
delta-hedged, held ~30d, on liquid ETFs, entered when a linear regression on three signals
predicts a return above a threshold. Supersedes [`../vrp_etf_model`](../vrp_etf_model).

**Why redone:** every number in the old study came from `sharpe_two/data/s2_dataset.pq`,
which is not a permitted input (Marco's R rebuild — see `../_data/README.md` and the
`straRetM1` trap). Marco also asked for the video's *original claims* and its *proposed
signals* to be retested, not just the strategy.

---

## Verdict (2026-07-22)

**The trade is real and the signals work. The old study's headline numbers do not survive,
because it never priced an option — it scored a formula.**

| Claim | Old study | This retest | |
|---|---|---|---|
| `log(IV/RV)` is the signal | ✅ IC +0.11 | **+0.120, t=7.2** (partial +0.118) | confirmed |
| `ivPctile1y` is useless | ❌ IC ~0 | IC **+0.007, t=0.4** on the proxy … but **Q1→Q5 +0.97pp** on the real trade | **was wrong** |
| flat/std fwd ratio is "context only" | ❌ IC −0.04 | **−0.059** on the proxy, **−1.54pp Q1→Q5** on the real trade | **understated — it's the best of the three** |
| the 3-feature regression | ❌ adds nothing | ❌ confirmed: beaten by a one-line rank-sort in every split | confirmed |
| IV/RV rank-sort, gated | **3.31× / Sharpe 2.04** | on the same span, **2.01× / Sharpe 0.65** | **not reproducible** |
| blind selling loses money | ✅ 0.95× | on the proxy yes (0.81×); **on real quotes it makes +1.04%/mo, Sharpe 1.16** | **artefact** |

### The finding that reframes everything

The old study's target was the variance-swap approximation

```
P&L / premium  ≈  1 − (RV/IV)²
```

`_data/strikes` now carries per-strike NBBO **and** per-strike delta, so the trade can just
be simulated: sell the real ATM call+put, rehedge daily at the archive's own delta, settle
at |S−K|. Against 8,213 such trades the proxy has **corr +0.60, R² 0.36** and is biased:
it says **+0.02%/trade**, the real trade pays **+1.03%/trade**. It also invents a left tail —
proxy min −150% of margin against the real −61% — because `1−(RV/IV)²` is unbounded below
while an actual hedged straddle is not.

So the proxy is a noisy, biased, fat-left-tailed stand-in, and every conclusion the old
study drew about *which signals work* was filtered through it. Two of the three signal
verdicts flip when you score the trade instead.

### Gross (figure `out/plots/42_vrp_gross.png`)

8,213 real delta-hedged short ATM straddles, 245 clean ETFs, 2013-01 → 2024-11, monthly
non-overlapping, equal weight, return on margin (0.20·spot + premium).

| book | mean/mo | **Sharpe** | final | maxDD | worst mo |
|---|---|---|---|---|---|
| blind: sell every ETF | +1.04% | 1.16 | 4.1× | −26% | −16.8% |
| IV/RV top 10% | +2.10% | 1.98 | 17.8× | −18% | −11.9% |
| volOfVol top 10% | +2.16% | 2.00 | 19.2× | −16% | −10.2% |
| backwardation top 10% | +2.34% | 1.95 | 24.2× | −30% | −22.1% |
| 3-signal blend top 20% | +2.98% | **2.23** | 57.9× | −24% | −21.1% |

Gross is positive in **12 of 12 years**. All four signals sort the real P&L monotonically
or near-monotonically; the strongest single sort is **backwardation** (signal 3 inverted),
which the video proposed with the wrong sign and the old study dismissed as context.

That signal is the forward factor of [`../ff_calendar_v2`](../ff_calendar_v2) wearing a
different name, and the two studies agree: front vol rich relative to the forward → sell
the front. Same conclusion reached from opposite directions.

### Cost (figure `out/plots/43_vrp_cost.png`) — read this second

Cost = sell both legs at the **bid** instead of mid, plus 2bp per share rehedge. There is
no exit spread: the position is held to expiry and settles at intrinsic.

Break-even fill fraction **f\* = gross ÷ cost** rises monotonically with liquidity:

| avgOptVolu20d | n | **f\*** | gross Sharpe |
|---|---|---|---|
| 2.5–10k | 2,791 | 0.47 | 0.57 |
| 10–50k | 2,909 | 0.64 | 0.57 |
| 50–250k | 1,807 | 0.76 | 0.44 |
| 250k–1M | 634 | **1.50** | 0.74 |
| 1M+ | 72 | **2.43** | 0.54 |

**Where the crossing actually is (2026-07-24).** Those buckets are coarse and 250k is just where
one of them happened to break. On a finer grid f\* crosses 1 between the 100–150k bucket (0.83)
and 150–250k (**1.27**) — i.e. around **~150k, not 250k**. Cumulative net Sharpe by cutoff:
0.18 at ≥100k (31 names), 0.31 at ≥150k (24), 0.41 at ≥200k (19), 0.42 at ≥250k (18), decaying
to 0.34 at ≥400k as the name count collapses. It is **flat over 150k–400k**, so the result is not
knife-edge on the parameter — but note these cutoffs were evaluated on the same 8,213 trades that
produced the original number, so "the crossing is near 150k" is defensible and "250k is optimal"
is not.

Volume is also only a **proxy** for what matters, the quoted half-spread, and a noisy one — the
250–500k bucket has a *higher* mean half-spread (4.30% of premium) than 150–250k (3.47%) despite
more volume. Measured half-spread by bucket: 10.28% (2.5–10k), 5.31% (25–50k), 3.55% (100–150k),
1.52% (500k–1M), 0.59% (1M+). Gating directly on quoted half-spread would be the cleaner rule.

Note the gross *Sharpe* is flat across the five buckets — liquidity does not make the edge
better, it makes it **payable**. Above f\*=1 you can cross the whole spread and still profit.

On the 18 ETFs at ≥250k contracts/day (in practice SPY, QQQ, IWM, EEM carry it — 513 of
706 trades), **net of the full spread**:

| book | mean/mo | **Sharpe** | final |
|---|---|---|---|
| gross, all liquid ETFs | +0.91% | 1.02 | 3.40× |
| **NET, all liquid ETFs** | **+0.36%** | **0.41** | 1.57× |
| **NET, blend top 50%** | **+0.61%** | **0.65** | 2.21× |

That is the honest answer: a real, cost-inclusive **~7%/yr on margin at Sharpe 0.65** on a
handful of ETFs — not Sharpe 2.04, not a 3.31× rank-sort, but not nothing either. Below
250k volume the spread takes all of it (net Sharpe −0.59 blind across the full ETF set).

---

## Restriking as a poor-man's delta hedge (figures 46/47)

The hedge is what makes this a variance trade, and it is **variance reduction, not alpha**: mean
return barely moves, std roughly halves (ETF Sharpe 0.38 → 1.18 unhedged → hedged, stock 0.98 →
1.87). If you cannot run the share leg, the alternative is to re-center the strike — buy the
drifted straddle back and sell a fresh ATM one on the same expiry whenever position delta
|2δ−1| breaches a band D. Continuous restriking → a delta hedge, gross.

`pipeline/restrike_pnl.py` sweeps D ∈ {naked, 0.9 … 0.1}, one chain-parse per entry pricing every
band, aligned to the hedged book's own entries so **naked reproduces its unhedged row exactly**
(that alignment is the validation — without it the sim priced a looser universe and naked printed
−0.08 instead of +0.38).

**Blind book** (every name, every month). ETF peaks loose: D=0.60 gives Sharpe 0.81, **53% of the
naked→hedge gap, for 2.1 restrikes**; D=0.10 gives 0.89 (64%) for 11.3. Stock peaks at **D=0.80,
Sharpe 1.44, 51% of the gap for 1.1 restrikes**, then *decays* — 10 restrikes gives a fifth of it
back, because each restrike crystallizes the adverse move and single-name drift does not revert
the way index drift does.

**Signal-filtered book** (blend top 20%) — the band preference is not the same:

| band | ETF Sh (%gap) | stock Sh (%gap) | rs/trade |
|---|---|---|---|
| naked | +0.98 | +2.53 | 0 |
| D=0.80 | +1.23 (21%) | +3.13 (49%) | ~1 |
| D=0.70 | +1.12 (12%) | **+3.23 (57%)** | 1.4 |
| D=0.30 | +1.46 (41%) | +3.12 (48%) | ~5 |
| D=0.10 | **+1.65 (57%)** | +3.19 (54%) | ~9 |
| share hedge | +2.16 | +3.75 | — |

On the filtered ETF book the loose-band advantage **reverses**: mean is flat across every band
(+2.44% naked → +2.48% at D=0.10), so restriking becomes pure variance reduction and more is
better. Screening removes the mean advantage that made D=0.60 special on the blind book.

**Practical verdict.** Cost is the tiebreak and it is not modelled here: a restrike crosses *two*
option spreads (buy back at ask, sell new at bid) against the share hedge's one cheap share
spread, so one restrike roughly **triples** total option-spread cost while lifting gross mean only
8–17%. Applied to the f\* table that pushes the liquid buckets under 1. So: **hedge with shares if
you can.** If you cannot, D≈0.7–0.8 on stocks is a genuinely good deal (one round-trip for half the
benefit); on ETFs restrike loosely or not at all. ⚠️ The sim stores mids only — the 3× multiplier is
arithmetic on spread structure, **not measured**. Recording bid/ask per restrike event is the open
item before acting on any band.

---

## Audit of the backtest's own assumptions (2026-07-24)

Checked after the fact; two came back clean, one found something undisclosed.

**Look-ahead in the split filter — real but immaterial, and the "fix" is worse.** `split_days()`
scales |return| by a rolling median over a **centred** 121-session window, i.e. up to 60 sessions
of future data decide whether today was a corporate action. At the trade level this changes **5 of
10,867 entries (0.05%)** — and all five go the *other* way: the causal version flags them.
All five are March 2020 (AMLP, GDXJ, JETS, OIH, USO), where a trailing median hasn't caught up to
the new vol regime so genuine crash returns trip `z>12`. Making the filter causal would **delete
five real short-straddle blow-ups** — the `straRetM1` mistake in miniature. Centred is kept
deliberately; corporate actions are announced in advance, so a live trader has the same
information, and the window is a cleaning device rather than a signal.

**Entry attrition — structural, not stress-related.** Only 76% of eligible monthly entries get
priced. Reasons: **no expiry in the 20–45 DTE window 19.0%**, no session in archive 3.3%, expiry
never settles 0.7%, no strike within 3% 0.6%, split-day 0.3%, **no two-sided ATM quote 0.07%**.
The dangerous mechanism — quotes going one-sided in a crash so the worst trades vanish — is 8
entries out of 10,867, and **March 2020 kept 65 of 76 trades (86%)**. The 19% is a calendar
artifact: entering on the month's first session, this month's third-Friday expiry is ~15–19 DTE
and next month's ~46–50, so names carrying **only monthly expiries** often have nothing in the
window while names with weeklies always do. Hence drop rates of 39–41% in the bottom two volume
quintiles against 5.5% in the top. Not a P&L bias, but the illiquid half of the universe is
sampled only when the calendar cooperates.

**⚠️ The strike archive has a 4½-month hole: 2021-01 → 2021-04 are 100% dropped, 2021-05 is 87%.**
Every result in this README and in figures 42–47 silently contains no trades for that window
(~3% of the 138-month sample, and not a random 3% — mid-2021 was suppressed index vol with
elevated single-name vol). The honest sample description is **"2013-01 → 2024-11 less a 4-month
hole in 2021."**

**Effective sample size is ~138, not 8,213.** Those trades occupy only 138 entry months and 981
distinct entry dates, ~60 entering the same session into the same market. Sharpe on monthly means
handles this correctly, but the SE on an annualized Sharpe from 138 months is ≈ **0.3** — so the
headline net 0.65 is about two standard errors from zero, and the difference between 0.65 and 0.42
is noise.

**Leveraged-ETF leak.** The hardcoded `LEVERED` exclusion set is from the 2013–2024 era and misses
the 2022+ single-stock leveraged wave entirely. 11 levered names leaked into the ETF backtest
(CHAU, CWEB, DFEN, DIG, DRN, JNUG, MVV, NAIL, ROM, TMF, URE) — but only **168 of 8,213 trades
(2.0%)**, moving blind Sharpe 1.18 → 1.17 and leaving blend top-20% at 2.16 either way. Published
numbers are unaffected. The *screener* was materially affected and now tests ORATS' `sector` field
(`contains "Leverag"`) instead of the stale list.

### Other known simplifications
- **Return is on *initial* margin**, held static, while real margin marks to market and expands
  exactly when the trade is losing. 0.8% of trades lose >20% of initial margin (worst −61%). No
  capital is charged for the share hedge either.
- **Equal weighting is unimplementable at the actionable book size** — margins per contract span
  10×, and the net book carries only ~2.8 concurrent positions from ~16 names.
- **`volOfVol` / `volOfIvol` construction is unverified.** Taken as raw ORATS columns; if either
  uses centred smoothing the signal is contaminated. vol-of-vol is the strongest leg on stocks.
- **Cohorts overlap** — mean hold is 32.3 calendar days against a 30.4-day month.
- Month-cohort ranking pools across entry days: 85% of ETF entries land on the month's first
  session, so ~15% are ranked against a slightly stale cross-section. Not look-ahead.

### Concurrency — how many positions are actually open

| book | avg open/day | median | p95 | max | entries/mo |
|---|---|---|---|---|---|
| ETF, every name every month | 62.3 | 59 | 122 | 233 | 59.5 |
| ETF, blend top 20% | 12.8 | 12 | 26 | 46 | 11.7 |
| **ETF, net book (≥250k + top 50%)** | **2.8** | 3 | 5 | 9 | 2.9 |
| stock, every name every month | 489.5 | 465 | 887 | 1,731 | 458 |
| stock, blend top 20% | 100.2 | 94 | 181 | 340 | 91 |

The actionable ETF book is **three straddles**. Diversification is doing almost nothing; the
single-name book is where a real portfolio exists — which is why its missing cost pass is the
highest-value open item.

### Not yet modelled — would reduce the net further
- **Early assignment.** The ITM leg is American and SPY/QQQ/EEM/IWM pay dividends; short
  ITM calls get assigned before ex-div. Held-to-expiry settlement assumes this away.
- **Commissions.** ~$1.30/straddle entry is ~0.014% of margin — negligible against 0.36%,
  but the share-leg commissions on ~23 rehedges are not, for small accounts.
- **Overnight gaps in the hedge.** Rehedging at the close only; the sim cannot do better.

---

## Data

Permitted inputs only.

- `cache/panel.parquet` — 6.48M rows, 6,491 tickers, **2007-01 → 2026-07**, built by
  `pipeline/build_panel.py` from `ORATS/core/*.csv.gz`. Every feature the old study used
  (`iv30d, iv60d, clsHv20d, ivPctile1y, avgOptVolu20d, confidence, pxCls, straPxM1,
  volOfVol, volOfIvol`) is a raw core column.
- `cache/real_trades_etf.parquet` — 8,213 simulated trades priced from `../_data/strikes`.

**The forward-RV target.** `fwd_rv(t) = clsHv20d(t + 21 sessions)` — ORATS's own trailing
20-day close-to-close HV, read 21 sessions later, is by definition the realized vol over
the following window. 12.4% of rows are dropped because that window is missing or spliced
across a gap; none are filled.

Rebuilding it from a price series instead would have been wrong: **`pxCls` in core is not
split-adjusted** (XLE prints −69.7% on 2025-12-08, a 2:1 split confirmed in `splits/`),
while ORATS's `clsHv20d` is. Validated against a hand-rolled forward vol on SPY/QQQ/IWM:
corr 0.991–0.993.

---

## Two traps this study had to handle

**1. Splits in the strike archive.** `stk` and `strike` are raw. A split inside a hold
looks like a 50% crash and destroys the hedge — an undetected EWT reverse split
(2016-11-07) produced a single −606%-of-margin trade and dragged the proxy-vs-real
correlation from 0.64 down to 0.45.

The fix must not be "drop moves bigger than X" — that deletes exactly the crashes a
short-vol study exists to measure, which is the `straRetM1` mistake. `split_days()` scores
each session against the name's **own local robust scale**:

```
z = |r_t| / (1.4826 · rolling-median |r|, 121 sessions, centred)
flag when z > 12 and |r| > 0.20
```

A 2:1 split scores z≈60; the worst genuine sessions in the sample score z<9 (EWA −14.9% in
March 2020 is z=8.8, SPY −8.7% is z=5.8). During a crash the denominator rises with the
numerator, so volatility itself can never trip it. Verified to catch EWT, USO 2020-04-29
and both UNG reverse splits while flagging nothing in the COVID crash. It also catches
2021-05-05, where the archive's 2021-01→04 hole closes and the "return" is four months long.

**2. The video's own metric is contaminated by the IV level.** `y_margin` divides by
`0.20·spot + straddle premium`, and the premium scales with IV — so high-IV names get their
return mechanically amplified. On ETFs the IV level's rank-IC is **−0.070** against
`y_risk` but **+0.186** against `y_margin`: a sign flip caused by the denominator, not by
anything in the market. Any signal correlated with IV level inherits that tilt. Every IC in
`signals.py` is therefore reported both raw and partialling the IV level out.

---

## Daily tool

`screener.py` — reads the latest ORATS core EOD file and ranks liquid ETFs as short 30d ATM
straddle candidates, using the study's own blend (mean pct-rank of `log(IV/RV)`, `volOfVol`,
and inverted 30/60 fwd-ratio = backwardation). The **actionable book** is the top-50% re-ranked
within whatever universe is in force. Carries a SPY term-structure regime banner (broad
backwardation = tail risk → size down the short book).

**Defaults (Marco's, 2026-07-24), which differ from the study's own universe:**

| | default | note |
|---|---|---|
| `--min-vol` | **10,000** | universe floor. The study used 2,500 |
| `--net-vol` | = `--min-vol` (**off**) | no liquidity gate on the actionable book. `--net-vol 250000` restores the study's original net book |
| `--drop-levered` | **off** | leveraged/inverse ETPs are **included**. The study excluded them (a 2× fund's RV is mechanically ~2× its constituents'), so they are untested territory, not validated picks |

Volume is reported per name rather than gated on, with a `SPREAD-RISK` tag below 100k/day — the
region where the measured net book was Sharpe-negative. Judge each name on its own quoted spread.
Leverage detection uses ORATS' `sector` field, not the stale hardcoded list.

It also prints `q_ivcv` — the rank of `iv_cv42` (42-session std/mean of iv30d), a faster
scale-free vol-of-vol than ORATS `volOfVol`. This is **diagnostic only, not in the blend**:
`analysis/fast_vov.py` showed it does not improve the net edge when swapped in (it survives the
{IV level, IV/RV} control slightly worse than `volOfVol`), but it is the best *standalone*
top-decile short sort (Sharpe 2.31 vs 2.01), so `q_ivcv` diverging from `q_vov` is worth seeing.
Computing it reads the trailing 60 core files (~6s); `--no-fast` skips it.

```
python3 screener.py                # latest date, all clean ETFs, blend-ranked
python3 screener.py --liquid-only  # only names that clear net (SPY/QQQ/IWM/EEM-tier)
python3 screener.py --no-fast      # skip the iv_cv42 diagnostic (no trailing-file read)
```

## Layout

```
screener.py                the daily tool — latest core EOD -> ranked short-straddle candidates
pipeline/build_panel.py    core day-files -> cache/panel.parquet (+ forward-RV target)
pipeline/real_pnl.py       [etf|stock] real delta-hedged straddles from ../_data/strikes
analysis/common.py         universe, filters, features, targets, rank-IC machinery
analysis/signals.py        the 3 video signals + volOfVol/volOfIvol -> out/signals*.csv
analysis/regression.py     walk-forward OLS vs rank-sorts -> out/books.csv
analysis/real_plots.py     figures 42 (gross) and 43 (cost)
pipeline/restrike_pnl.py   [etf|stock] delta-band restrike sweep -> cache/restrike_<kind>.parquet
analysis/restrike_books.py book selection shared by the restrike figures (all|top20|top10|net)
analysis/restrike_plot.py  figure 46 — Sharpe & std vs restrike band
analysis/restrike_equity.py figure 47 — equity curve per band
analysis/leg_decay.py      per-leg decay of the three signals on real quotes
analysis/leg_decay_core.py the same, extended past the strike archive via the core panel
analysis/audit_lookahead_attrition.py  the split-filter look-ahead + entry-attrition audit
```

Run everything with `/home/marco/trading/.venv_orats/bin/python`, scripts from their own
directory. `build_panel.py` ~90s; `signals.py` ~8 min; `real_pnl.py etf` ~40s.

## Open
- **Cost pass on single names.** The highest-value open item. The stock book is 63,214 trades
  with ~100 concurrent positions and gross Sharpe 3.23 (blend top-20%, restruck) — but it has
  never had an f\*-by-liquidity analysis, so it is unknown whether that is a real portfolio or
  an artifact of un-tradeable names. Everything ETF-side says execution decides.
- **Measured restrike cost.** `restrike_pnl.py` stores mids only; record bid/ask at each restrike
  event so the option-spread cost is measured rather than inferred from spread structure.
- Single names (`real_pnl.py stock`, 107k entries) — the proxy-based ICs are stronger there
  (volOfVol partial +0.087 vs +0.046 on ETFs), and the same proxy-vs-real gap needs checking.
- The liquid-ETF net result rests on 4 names. Extending `../_data/strikes` past 2024-12-20
  is the natural out-of-sample test.
- No costs figure exists yet for the walk-forward *regression* books — they are proxy-scored
  and superseded by the real-quote books, kept only to answer "does the regression help".
