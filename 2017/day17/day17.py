#!/usr/bin/env python

steps = 303
curl = 1
pos = 0
out = 0
for i in range(50000000):
    to_ins = i+1
    new = (pos + steps) % curl
    new += 1
    if new == 1:
        out = to_ins
    pos = new
    curl += 1

print(out)

