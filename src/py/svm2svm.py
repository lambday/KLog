#!/usr/bin/env python

# convert libsvm file compressing indices

# python svm2svm.py file.libsvm > newfile.libsvm
from __future__ import print_function
import string, os, sys


def svm2svm(lines):
    gf = 1
    remap = {}
    for line in lines:
        data_comment = line.strip().split('#')
        items = data_comment[0].split()
        print('%s' % items[0], end='')
        phi = []
        for pair in range(1,len(items)):
            (feature,val) = items[pair].split(':')
            if feature not in remap:
                gf = gf + 1
                remap[feature] = gf
            phi.append((remap[feature],float(val)))
        phi.sort()
        for p in phi:
            print(' %d:%f' % (p[0],p[1]), end='')
        print(' # %s' % data_comment[1])


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Usage: %s file.libsvm' % sys.argv[0])
    else:
        lines = open(sys.argv[1]).readlines()
        svm2svm(lines)
