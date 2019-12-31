#!/usr/bin/env python

import sys
import struct

with open(sys.argv[1]) as f:
    with open(sys.argv[2], 'wb') as o:
        for line in f.readlines():
            if 'byte' in line:
                bs = list(map(eval, [b.replace('%', '0b') for b in line.strip().split(' ')[1].split(',')]))
                print(bs)
                [o.write(struct.pack('B', b)) for b in bs]
