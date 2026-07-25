#!/usr/bin/env python3
"""
Extract straddles / strangles for given tickers from an Interactive Brokers
activity-statement PDF ("Dettaglio eseguiti" executions blotter), to CSV.

A straddle/strangle is detected STRICTLY as:
  * an opening CALL and an opening PUT filled at the SAME timestamp, same
    direction (both bought = long, both sold = short) — an IB combo fill.
      same strike -> straddle, different strikes -> strangle.
  * the EXIT is the close event that closes BOTH of those legs together.

Anything else in the account (put/call spreads, verticals, rolls, single legs,
legged entries/exits) is NOT a straddle and is deliberately skipped — better to
omit than to mis-pair. Skipped-but-nearby structures are listed as a footnote so
nothing is silently lost. Only exchange-summary rows ("-") are read.

Output columns (Profit is GROSS; Cost is commissions, kept separate):
  ticker, Positions, DateEntry, DateExit, PremiumEntry, PremiumExit, Profit, Cost

Usage:  python3 extract_straddles.py AI AAPL AMD MU
        python3 extract_straddles.py --file <stmt.pdf> --out mine.csv AI
"""
import argparse
import csv as csvmod
import re
import subprocess
import sys
from collections import defaultdict
from types import SimpleNamespace as NS

ROW = re.compile(
    r'^\s*\S+\s+'
    r'([A-Za-z0-9.]+)\s+(\d{2}[A-Z]{3}\d{2})\s+([\d.]+)\s+([CP])\s+'   # ticker exp strike right
    r'(\d{4}-\d{2}-\d{2}),\s*(\d{2}:\d{2}:\d{2})\s+'                    # trade date, time
    r'\d{4}-\d{2}-\d{2}\s+'                                            # settlement (skip)
    r'(\S+)\s+'                                                        # exchange
    r'(BUY|SELL)\s+'
    r'(-?\d+)\s+'                                                      # quantity
    r'[\d.]+\s+'                                                       # price (skip)
    r'(-?[\d,]+\.\d+)\s+'                                              # proceeds
    r'(-?[\d.]+)\s+'                                                   # commission
    r'-?[\d.]+\s+\S+\s+'                                              # commissione, order type (skip)
    r'([A-Z;]+)\s*$'                                                   # code (O / C)
)


def money(s):
    return float(s.replace(",", ""))


def parse_execs(pdf, tickers):
    txt = subprocess.run(["pdftotext", "-layout", pdf, "-"],
                         capture_output=True, text=True).stdout
    tickers = {t.upper() for t in tickers}
    execs = []
    for line in txt.splitlines():
        m = ROW.match(line)
        if not m:
            continue
        tkr, exp, strike, right, d, t, exch, side, qty, proceeds, comm, code = m.groups()
        if tkr not in tickers or exch != "-":       # order-summary rows only
            continue
        codes = code.split(";")
        execs.append(NS(
            ticker=tkr, key=(tkr, exp, float(strike), right), right=right,
            dt=f"{d} {t}", date=d, sign=1 if side == "BUY" else -1,
            qty=abs(int(qty)), cash=money(proceeds), comm=money(comm),
            is_open="O" in codes, is_close="C" in codes))
    return execs


def build(execs):
    # close events grouped by (ticker, timestamp): {key: [cash, comm]}
    ce = defaultdict(lambda: defaultdict(lambda: [0.0, 0.0]))
    for e in execs:
        if e.is_close:
            ce[(e.ticker, e.dt)][e.key][0] += e.cash
            ce[(e.ticker, e.dt)][e.key][1] += e.comm
    close_events = sorted((NS(ticker=t, dt=dt, keys=dict(k)) for (t, dt), k in ce.items()),
                          key=lambda x: x.dt)

    # opening executions grouped by (ticker, timestamp)
    oe = defaultdict(list)
    for e in execs:
        if e.is_open:
            oe[(e.ticker, e.dt)].append(e)

    used, positions, skipped = set(), [], []
    for (tkr, odt), legs in sorted(oe.items(), key=lambda x: x[0][1]):
        calls = [l for l in legs if l.right == "C"]
        puts = [l for l in legs if l.right == "P"]
        if not (len(calls) == 1 and len(puts) == 1 and calls[0].sign == puts[0].sign):
            if legs:                                 # note the non-straddle structure
                ks = "+".join(sorted(f"{l.key[2]:g}{l.right}" for l in legs))
                skipped.append((tkr, odt.split()[0], ks))
            continue
        ck, pk = calls[0].key, puts[0].key
        entry_cash = calls[0].cash + puts[0].cash
        entry_comm = calls[0].comm + puts[0].comm
        # exit = first unused close event after the open that closes BOTH legs
        exit_cash = exit_comm = 0.0
        exit_dt = None
        for i, c in enumerate(close_events):
            if i in used or c.ticker != tkr or c.dt <= odt:
                continue
            if ck in c.keys and pk in c.keys:
                used.add(i)
                exit_cash = c.keys[ck][0] + c.keys[pk][0]
                exit_comm = c.keys[ck][1] + c.keys[pk][1]
                exit_dt = c.dt
                break
        closed = exit_dt is not None
        positions.append(NS(
            ticker=tkr, kind="straddle" if ck[2] == pk[2] else "strangle",
            expiry=ck[1], strikes=sorted({ck[2], pk[2]}),
            positions=min(calls[0].qty, puts[0].qty),
            entry_dt=odt.split()[0], exit_dt=exit_dt.split()[0] if closed else "OPEN",
            premium_entry=abs(entry_cash),
            premium_exit=abs(exit_cash) if closed else None,
            profit=(entry_cash + exit_cash) if closed else None,
            cost=abs(entry_comm + (exit_comm if closed else 0.0)), closed=closed))
    return positions, skipped


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("tickers", nargs="+")
    ap.add_argument("--file", default="/home/marco/Downloads/U7067741_20260401_20260723.pdf")
    ap.add_argument("--out", default="straddles.csv")
    a = ap.parse_args()

    execs = parse_execs(a.file, a.tickers)
    positions, skipped = build(execs)
    order = {t.upper(): i for i, t in enumerate(a.tickers)}
    positions.sort(key=lambda s: (order.get(s.ticker, 99), s.entry_dt))

    cols = ["ticker", "Positions", "DateEntry", "DateExit", "PremiumEntry",
            "PremiumExit", "Profit", "Cost"]

    def rec(s):
        return [s.ticker, s.positions, s.entry_dt, s.exit_dt,
                f"{s.premium_entry:.2f}",
                f"{s.premium_exit:.2f}" if s.closed else "OPEN",
                f"{s.profit:.2f}" if s.closed else "OPEN", f"{s.cost:.2f}"]

    with open(a.out, "w", newline="") as f:
        w = csvmod.writer(f)
        w.writerow(cols)
        w.writerows(rec(s) for s in positions)

    print("\t".join(cols))
    for s in positions:
        print("\t".join(str(x) for x in rec(s)))
    print(f"\nwrote {len(positions)} straddle/strangle positions to {a.out}")
    miss = [t for t in a.tickers if t.upper() not in {s.ticker.upper() for s in positions}]
    if miss:
        print(f"  no straddles/strangles for: {', '.join(miss)}")
    if skipped:
        print(f"  skipped {len(skipped)} non-straddle structures (spreads/rolls/singles), e.g.:")
        for tkr, d, ks in skipped[:8]:
            print(f"    {tkr} {d}  {ks}")


if __name__ == "__main__":
    main()
