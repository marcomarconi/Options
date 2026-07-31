# Regime Timeline — replication of the Sharpe Two "Regime Timeline" charts

Replicates the per-ticker vol-regime classification shown in the Sharpe Two
*Forward Note* PDFs (`/home/marco/s3_fn`): daily 30DTE implied vol scatter,
each day colored by regime (Low RV, Positive VRP, Calm, Post Stress, Stress,
High Stress, Extreme Stress). Their method is proprietary; this is a
reconstruction from ORATS data that reproduces the qualitative behavior.

## Method

1. **Features** (per ticker, daily, from `ORATS_core.pq`; 30DTE tenor uses
   `iv30d` vs `orHv20d`, 9DTE uses `iv10d` vs `orHv5d`):
   - `iv_pct`, `rv_pct` — trailing-2y percentile ranks of log IV / log RV
     (makes features comparable across tickers with different vol levels)
   - `vrp` — log(IV/RV)
   - `iv_off_peak` — log IV minus its trailing-1m max (vol-crush detector)
   - `iv_mom` — 10-day change in log IV
   - `stress_recency` — trailing-3m max of `iv_pct`
2. **Classifier**: diagonal-covariance Gaussian mixture (own EM, k=7,
   k-means++ init, seed 42) fit on the pooled standardized panel of 15 liquid
   ETFs (SPY QQQ IWM DIA GLD TLT USO XLE EEM FXI SLV HYG EFA XLF SMH),
   2016→present, ~38k ticker-days.
3. **Labels**: clusters are auto-named from their centroids (rules in
   `label_centroids`): elevated + compressed-VRP/falling → Post Stress;
   elevated + rich VRP/rising → Stress; top decile of both IV and RV → High
   Stress; within High Stress, days with `iv_pct>0.99` (or >0.97 with
   `rv_pct>0.9`) are flagged Extreme Stress; low-everything → Low RV; quiet
   with rich VRP → Positive VRP; residual → Calm.

## Results

- `figures/replication_qqq_uso.png` — same ticker/window pairs as the
  Forward Notes 20260628 (QQQ) and 20260315 (USO). Matches the published
  charts well: purple/blue quiet phases, yellow on the post-spike descents,
  orange at the spikes, red only at genuine tails (USO 90–130 in Mar 2026).
- `figures/sanity_spy_history.png` — out-of-window check on known history:
  2017 = Low RV (green), Volmageddon and Q4-2018 = red/brown with yellow
  decay, COVID = Extreme Stress at IV 75 → months of Post Stress → back to
  Calm/Low RV by mid-2021.
- Daily regime switch rate ~10–13%/day (no temporal smoothing applied; the
  published charts also show single-day color changes).

## Usage

```
cd src
python regime_model.py            # refit both tenors, writes results/regimes_{30dte,9dte}.pq
python plot_timeline.py           # replication panels vs the PDFs
python plot_timeline.py TLT       # any basket ticker, last 12 months, 30dte
python plot_timeline.py TLT 9dte
```

## R port

`src/regime_timeline.R` is a standalone R implementation of the same
pipeline (arrow + dplyr/slider features, mclust `VVI` G=7 GMM with random-
subset initialization — mclust's hierarchical init doesn't scale to ~40k
rows — same centroid label rules, ggplot2/patchwork figures saved with an
`_r` suffix, labeled panel to `results/regimes_30dte_r.pq`):

```
cd src
Rscript regime_timeline.R         # fit 30dte + replication/sanity figures
Rscript regime_timeline.R TLT     # + single-ticker figure
```

### Shiny integration

`src/regime_shiny.R` is a self-contained scoring module used by the Options
Shiny app (`Shiny/ui.R`, "Ticker" tab, output `ticker_plot_regime`). It loads
the fitted GMM bundle (`results/regime_gmm_30dte.rds`, written by
`regime_timeline.R`), reads the full-history research parquet for the
requested ticker (arrow filter pushdown, ~0.5 s), rebuilds the features,
scores with `mclust::predict.Mclust`, and returns a native `plot_ly` chart
(the app's ggplot2 4.x breaks `ggplotly()`). Works for ANY ORATS ticker, not
just the training basket (scored-vs-fitted agreement on basket tickers
~99.9%). Includes a despike guard (drops days where IV is >4x / <0.25x its
trailing 3-week median — bad prints like NVDA 2025-04 iv30d=380; genuine
spikes like COVID ramp gradually and survive). Caveats: IVs are not
earnings-adjusted, so single names can print Stress into earnings weeks; the
plot's history ends at the research parquet's last date (no delayed-feed
append, unlike the app's other panels).

Python and R converge to *different but equivalent* EM solutions (mixture
likelihoods are multimodal), so day labels agree 58% exactly and 76% at the
quiet/post/stress group level; disagreements are essentially all between
adjacent regimes (Calm↔Positive VRP, Stress↔Positive VRP at the 0.6 iv_pct
boundary), never quiet↔stress. The deterministic day-level Extreme Stress
rule agrees ~100%. The label rules were softened (High Stress gate
0.85/0.85, Post Stress gate iv_pct>0.5, Extreme override on any
stress-family day with an RV guard) precisely so both fits label their
clusters consistently. For bit-identical regimes across languages, export
the fitted Python GMM parameters and score in R instead of refitting.

## Caveats

- Unsupervised with hand-tuned label rules on the centroids: the *boundaries*
  between adjacent stress shades are calibration choices, not discovered
  truth. Re-fitting with a different seed/k can shuffle cluster boundaries;
  the label rules are written against centroid features, so names survive
  refits, but individual borderline days can flip.
- Percentile features need ~2y warm-up: series start in 2016.
- Trained on the 15-ETF basket only; applying to single names would need the
  basket extended (earnings distortions → use `exErnIv30d`/`orHvXern20d`).
- ORATS data ends 2026-05-29 at the time of writing, so the June-2026 QQQ
  episode in FN-20260628 is not fully covered.
