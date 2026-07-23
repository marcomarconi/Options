"""
Split both candidate books by option-listing type of the underlying at ENTRY:
  - WEEKLY names: ticker had at least one non-monthly expiry listed
    1-40 days out on the entry date
  - MONTHLY-ONLY names: only standard (3rd-Friday +/-1d, incl. legacy
    Saturday) expiries listed

Books: A. lag-2 ungated   B. lag-15 + 4-signal walk-forward model (OOS 2016+)

Builds cache/weekly_flags.parquet (per ticker x tradeDate) from the smv
strike cache on first run (~200 tickers). Writes fig 14.
Run from analysis/:  python3 weekly_split.py
"""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from oq_model import build_model_book, FIRST_TEST_YEAR

R = "ret_spot_mid"
OUT = "../figures"
SMV = "../../ff_calendar_v2/cache/smv_single"
FLAGS = "../cache/weekly_flags.parquet"
BLUE, VIOLET, RED = "#4878cf", "#4a3aa7", "#d1615d"


def tstat(r):
    return r.mean() / r.std() * np.sqrt(len(r))


def third_friday(y, m):
    d = pd.Timestamp(y, m, 1)
    fridays = pd.date_range(d, d + pd.offsets.MonthEnd(0), freq="W-FRI")
    return fridays[2]


def is_monthly(expir):
    """within +/-1 day of the 3rd Friday (covers legacy Saturday-dated
    monthlies and holiday-shifted Thursdays)"""
    tf = pd.DatetimeIndex([third_friday(e.year, e.month) for e in expir])
    return np.abs((expir - tf) / pd.Timedelta(days=1)) <= 1


def build_flags():
    rows = []
    tickers = sorted(os.listdir(SMV))
    for i, tkr in enumerate(tickers):
        f = f"{SMV}/{tkr}/part-0.parquet"
        if not os.path.exists(f):
            continue
        d = pd.read_parquet(f, columns=["tdate", "expir"]).drop_duplicates()
        d["tdate"] = pd.to_datetime(d.tdate)
        d["expir"] = pd.to_datetime(d.expir)
        dte = (d.expir - d.tdate).dt.days
        d = d[(dte >= 1) & (dte <= 40)]
        uniq = d.expir.drop_duplicates()
        monthly_map = dict(zip(uniq, is_monthly(pd.DatetimeIndex(uniq))))
        d["weekly_exp"] = ~d.expir.map(monthly_map)
        g = d.groupby("tdate").weekly_exp.any().rename("has_weekly")
        rows.append(g.reset_index().assign(ticker=tkr))
        if (i + 1) % 50 == 0:
            print(f"  scanned {i+1}/{len(tickers)} tickers")
    flags = pd.concat(rows, ignore_index=True)
    flags.to_parquet(FLAGS)
    return flags


def load_flags():
    if os.path.exists(FLAGS):
        return pd.read_parquet(FLAGS)
    return build_flags()


def split_stats(d, lab):
    print(f"\n== {lab} ==")
    out = []
    for name, s in [("weekly names", d[d.has_weekly == True]),      # noqa
                    ("monthly-only", d[d.has_weekly == False])]:
        r = s[R]
        exp_dte = (s.expir - s.entry).dt.days
        print(f"{name:13s} n={len(s):5d}  {r.mean()*100:+.3f}%/spot  "
              f"t={tstat(r):+.2f}  win {(r>0).mean()*100:.0f}%  "
              f"median trade-expiry DTE {exp_dte.median():.0f}d  "
              f"median debit/spot {(s.debit_mid/s.spot).median()*100:.1f}%")
        out.append((name, len(s), r.mean() * 100, tstat(r)))
    return out


def main():
    os.makedirs(OUT, exist_ok=True)
    flags = load_flags()
    flags = flags.rename(columns={"tdate": "entry"})

    ls = pd.read_parquet("../cache/trades_prern_lagscan.parquet")
    lag2 = ls[ls.lag == 2].merge(flags, on=["ticker", "entry"], how="left")
    model, _ = build_model_book()
    model = model.merge(flags, on=["ticker", "entry"], how="left")

    for d, lab in [(lag2, "lag2"), (model, "model")]:
        miss = d.has_weekly.isna().mean()
        print(f"{lab}: unmatched entry-date flags {miss*100:.2f}%")
    lag2 = lag2.dropna(subset=["has_weekly"])
    model = model.dropna(subset=["has_weekly"])
    print(f"universe mix (lag2 trades): weekly {lag2.has_weekly.mean()*100:.0f}%"
          f" / monthly-only {(1-lag2.has_weekly.mean())*100:.0f}%")
    print("weekly share by year (lag2):")
    print((lag2.groupby(lag2.entry.dt.year).has_weekly.mean() * 100)
          .round(0).astype(int).to_string())

    a = split_stats(lag2, f"lag 2 ungated (n={len(lag2)})")
    a2 = split_stats(lag2[lag2.entry.dt.year >= FIRST_TEST_YEAR],
                     f"lag 2 ungated, {FIRST_TEST_YEAR}+ (era control)")
    b = split_stats(model, f"lag 15 + 4-signal model, OOS {FIRST_TEST_YEAR}+ "
                           f"(n={len(model)})")

    fig, axes = plt.subplots(1, 2, figsize=(11.5, 4.6))
    for ax, rows, title, c in [
            (axes[0], a, f"lag 2 ungated (n={len(lag2)})", BLUE),
            (axes[1], b, f"lag 15 + 4-signal model, OOS 2016+ "
                         f"(n={len(model)})", VIOLET)]:
        means = [m for _, _, m, _ in rows]
        ax.bar(np.arange(2), means,
               color=[c if m >= 0 else RED for m in means], width=0.55)
        for xi, (name, n, m, t) in enumerate(rows):
            ax.text(xi, m, f"t={t:.1f}\nn={n}", ha="center",
                    va="bottom" if m >= 0 else "top", fontsize=8.5,
                    color="#52514e")
        ax.set_xticks(np.arange(2), [name for name, *_ in rows], fontsize=9.5)
        lo, hi = min(0, min(means)), max(0, max(means))
        pad = 0.22 * (hi - lo)
        ax.set_ylim(lo - (pad if lo < 0 else 0), hi + pad)
        ax.axhline(0, color="k", lw=0.7)
        ax.set_ylabel("mean %/spot (gross mid)")
        ax.set_title(title, fontsize=10.5)
    fig.suptitle("weekly-listed names vs monthly-only names "
                 "(classified at entry date)", fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    fig.savefig(f"{OUT}/14_weekly_split.png", dpi=110)
    print(f"\nwrote 14_weekly_split.png to {OUT}/")


if __name__ == "__main__":
    main()
