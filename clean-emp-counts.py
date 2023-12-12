#!/usr/bin/env python3
import sys

done = set()
with open(sys.argv[1]) as f:
    for line in f:
        l = line.strip().split(',')

        cid = l[0]

        if cid not in done:
            print(line.strip())
            done.add(cid)
