#!/usr/bin/env python

import sys
from itertools import islice
from typing import Iterator
from functools import reduce
from collections import defaultdict

def main():
    input = map(int, open(sys.argv[1]).read().strip().splitlines())
    secret_seqs = map(evolve_secret, input)
    price_seqs = map(lambda seq: map(price, seq), secret_seqs)
    change_seqs = map(lambda seq: (sequence4s(changes(seq))), price_seqs)
    # seq_to_price = [{
    #     seq: price
    #     for (seq, price) in zip(islice(seqs, 1997), islice(prices, 4, None))
    # } for (seqs, prices) in zip(change_seqs, price_seqs)]
    # all_keys = reduce(lambda x, y: x | y.keys(), seq_to_price, set())
    # totals = {
    #     k: sum([m.get(k, 0) for m in seq_to_price])
    #     for k in all_keys
    # }
    totals = compute_totals(change_seqs, price_seqs)
    print(max(totals.values()))

def evolve_secret(num):
    while True:
        num = step(num)
        yield num

def step(x):
    x = prune(mix(x, x << 6))
    x = prune(mix(x, x >> 5))
    x = prune(mix(x, x << 11))
    return x

def prune(x):
    return x & 16777215

def mix(x, y):
    return x ^ y

def price(x):
    return x % 10

def changes(seq: Iterator[int]):
    return (b - a for a, b in zip(seq, islice(seq, 1, None)))

def sequence4s(seq):
    return (
        (a+10)*8000 + (b+10)*400 + (c+10)*20 + d
        for a, b, c, d in
        zip(seq, islice(seq, 1, None), islice(seq, 2, None), islice(seq, 3, None))
    )

def compute_totals(change_seqs, price_seqs):
    totals = defaultdict(lambda: 0)
    for changes, prices in zip(change_seqs, price_seqs):
        seen_seqs = set()
        for change, price in zip (islice(changes, 1997), islice(prices, 4, None)):
            if change in seen_seqs:
                continue
            seen_seqs.add(change)
            totals[change] += price
    return totals

if __name__ == '__main__':
    main()
