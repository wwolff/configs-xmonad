#!/bin/sh
#for i in `find *x[bp]m` do echo "$i: ^i($i)" done | dzen2 -ta l -h 32 -w 700 -l 20 -x 100 -y 220 -p
find *x[bp]m -exec echo '{}'": ^i("'{}'")" \; | dzen2 -ta l -h 32 -w 700 -l 20 -x 100 -y 220 -p
