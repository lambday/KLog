#!/usr/bin/env python -i
from klogpy.rpplot import rp
import sys,pylab


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Usage: %s results dir1 [dir2 [dir3 ...]]' % sys.argv[0])
        exit(1)
    methods = []
    for klogdir in sys.argv[1:]:
        print(klogdir.split('/'))
        print(klogdir.split('/')[-1])
        # methods.append((klogdir+'/output.yyy.pr', klogdir.split('/')[-1]))
        target = open(klogdir+'/target.txt').readlines()[0].strip()
        methods.append((klogdir+'/output.yyy.pr', target+' pr'))
        methods.append((klogdir+'/output.yyy.roc', target+' roc'))
    fig=pylab.figure()
    rp(methods, fig,111,'All')
    fig.show()

    
