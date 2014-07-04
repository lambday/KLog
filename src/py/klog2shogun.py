#!/usr/bin/env python

# convert klog-generated feature files in a format suitable for shogun (compressing indices)

# python klog2shogun file.libsvm features_file.dat labels_file.dat
from __future__ import print_function
import string, os, sys
import random

def klog2shogun(lines,features_file,labels_file,subsample):
    n_features = 1
    remap = {}
    ave_len=0
    for line in lines:
        data_comment = line.strip().split('#')
        items = data_comment[0].split()
        phi = []
        for pair in range(1,len(items)):
            (feature,val) = items[pair].split(':')
            if feature not in remap:
                n_features += 1
                remap[feature] = n_features
            phi.append((remap[feature],float(val)))
        phi.sort()
        ave_len = ave_len+len(phi)
        if subsample is not None:
            if items[0]=='-1' and random.random() > subsample:
                continue
        print('%s' % items[0], end='',file=features_file)
        print('%s' % items[0], file=labels_file)
        for p in phi:
            print(' %d:%f' % (p[0],p[1]),end='',file=features_file)
        print(file=features_file)
        # print(' # %s' % data_comment[1])
    ave_len = float(ave_len) / len(lines)
    print("Done. Total features = %d, Average len = %.1f, # Cases = %d " % (n_features,ave_len,len(lines)))

if __name__ == '__main__':
    if len(sys.argv) < 4:
        print('Usage: %s file.libsvm features_file.dat labels_file.dat [subsample neg %%]' % sys.argv[0])
    else:
        lines = open(sys.argv[1]).readlines()
        features_file = open(sys.argv[2],'w')
        labels_file = open(sys.argv[3],'w')
        subsample=None
        if len(sys.argv)==5:
            subsample=float(sys.argv[4])
        klog2shogun(lines,features_file,labels_file,subsample)
        features_file.close()
        labels_file.close()
