#!/usr/bin/env python3
"""
Retest of the Option-Quants VRP video's three proposed signals, plus the vol-of-vol pair
added later, on a panel rebuilt from ORATS core files only.

The video claims: sell 30d ATM delta-hedged straddles on liquid ETFs, entered when a
linear regression on three features predicts a return above a threshold.
  1. log(IV/RV)      -- IV rich vs the name's own realized
  2. ivPctile1y      -- "sell when IV is low in its own 1y range"
  3. flat-forward / standard-forward ratio (30/60d)

Each is scored by per-day cross-sectional rank-IC against the target, raw and partialling
out the IV LEVEL -- because a signal that only reproduces "this name has high IV" is not a
signal, it is a beta. t-stats deflated by sqrt(21) for the target's overlap.

Run: /home/marco/trading/.venv_orats/bin/python signals.py
Writes out/signals.csv, out/signals_by_year.csv, out/deciles.csv
"""
import numpy as np
import pandas as pd

from common import BASE, load, daily_ic, ic_summary, DEFLATE

SIGS = [("f_ivrv", "1  log(IV/RV)"),
        ("f_ivpct", "2  ivPctile1y"),
        ("f_ffr", "3  flat/std fwd ratio"),
        ("f_vov", "+  volOfVol"),
        ("f_voiv", "+  volOfIvol"),
        ("f_ivlvl", "control: IV level")]


def block(d, label, y, rows):
    print(f"\n{'='*86}\n  {label}   target = {y}   n={len(d):,}  "
          f"{d.tradeDate.min().date()}..{d.tradeDate.max().date()}  "
          f"{d.ticker.nunique()} tickers\n{'='*86}")
    print(f"{'signal':24s}{'raw IC':>9s}{'t':>7s}{'|IVlvl IC':>11s}{'t':>7s}"
          f"{'| pos days':>11s}{'names/day':>11s}")
    for col, name in SIGS:
        raw = ic_summary(daily_ic(d, col, y))
        if col == "f_ivlvl":
            par = dict(ic=np.nan, t=np.nan)
        else:
            par = ic_summary(daily_ic(d, col, y, controls=["f_ivlvl"]))
        print(f"{name:24s}{raw['ic']:>+9.3f}{raw['t']:>7.1f}"
              f"{par['ic']:>+11.3f}{par['t']:>7.1f}"
              f"{100*raw['pos']:>10.0f}%{raw['names']:>11.0f}")
        rows.append(dict(universe=label, target=y, signal=name, ic=raw["ic"], t=raw["t"],
                         ic_partial=par["ic"], t_partial=par["t"], pos=raw["pos"],
                         days=raw["days"], names=raw["names"]))


def orthogonality(d, y):
    """Does vol-of-vol survive controlling for BOTH the IV level and IV/RV?
    That is the claim that made it a second signal."""
    print(f"\n  orthogonality check (target {y}) -- partial IC | {{IV level, IV/RV}}")
    for col, name in [("f_vov", "volOfVol"), ("f_voiv", "volOfIvol"),
                      ("f_ivpct", "ivPctile1y"), ("f_ffr", "flat/std fwd")]:
        s = ic_summary(daily_ic(d, col, y, controls=["f_ivlvl", "f_ivrv"]))
        print(f"    {name:14s} {s['ic']:+.3f}  t={s['t']:.1f}  pos {100*s['pos']:.0f}%")


def by_year(d, y, out):
    print(f"\n  per-year rank-IC (target {y}) -- the stability the old study flagged as shaky")
    cols = ["f_ivrv", "f_vov", "f_ivpct", "f_ffr"]
    ics = {c: daily_ic(d, c, y).set_index("tradeDate").ic for c in cols}
    print(f"    {'year':6s}" + "".join(f"{c.replace('f_',''):>12s}" for c in cols))
    for yr, idx in pd.Series(1, index=ics[cols[0]].index).groupby(
            lambda t: t.year).groups.items():
        row = {c: ics[c].reindex(idx).mean() for c in cols}
        print(f"    {yr:<6d}" + "".join(f"{row[c]:>+12.3f}" for c in cols))
        out.append(dict(target=y, year=yr, **{c: row[c] for c in cols}))


def deciles(d, sig, y, out, label):
    """Decile sort: the IC says 'ordered', this says 'and by how much, monotonically?'"""
    d = d.copy()
    d["dec"] = d.groupby("tradeDate")[sig].transform(
        lambda s: pd.qcut(s.rank(method="first"), 10, labels=False, duplicates="drop")
        if s.nunique() >= 10 else np.nan)
    g = d.dropna(subset=["dec"]).groupby("dec")[y]
    t = pd.DataFrame({"mean": 100 * g.mean(), "n": g.size(),
                      "sharpe_tr": g.mean() / g.std()})
    # per-trade Sharpe x sqrt(12): one non-overlapping 30d trade per month
    t["sharpe_yr"] = t.sharpe_tr * np.sqrt(12)
    t["win%"] = 100 * d.dropna(subset=["dec"]).groupby("dec")[y].apply(lambda s: (s > 0).mean())
    print(f"\n  deciles of {sig} vs {y} ({label})  [D0 = lowest signal]")
    print(t.round(3).to_string())
    for dec, r in t.iterrows():
        out.append(dict(universe=label, signal=sig, target=y, decile=int(dec), **r.to_dict()))
    return t


def main():
    etf = load("etf")
    stk = load("stock")

    rows, yrs, dec = [], [], []
    for d, label in [(etf, "clean liquid ETFs"), (stk, "single names")]:
        for y in ["y_risk", "y_margin"]:
            block(d, label, y, rows)
        orthogonality(d, "y_risk")
    by_year(etf, "y_risk", yrs)
    by_year(stk, "y_risk", yrs)

    for d, label in [(etf, "clean liquid ETFs"), (stk, "single names")]:
        deciles(d, "f_ivrv", "y_margin", dec, label)
    deciles(etf, "f_ivpct", "y_margin", dec, "clean liquid ETFs")
    deciles(etf, "f_ffr", "y_margin", dec, "clean liquid ETFs")

    pd.DataFrame(rows).to_csv(f"{BASE}/out/signals.csv", index=False)
    pd.DataFrame(yrs).to_csv(f"{BASE}/out/signals_by_year.csv", index=False)
    pd.DataFrame(dec).to_csv(f"{BASE}/out/deciles.csv", index=False)
    print(f"\nwrote out/signals.csv, out/signals_by_year.csv, out/deciles.csv "
          f"(t deflated by sqrt(21)={DEFLATE:.2f})")


if __name__ == "__main__":
    main()
