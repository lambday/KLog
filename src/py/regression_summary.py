from __future__ import print_function
import numpy as np
import os,sys

"""
Given a kLog result directory, average and compute standard deviations of various performance measures for regression
"""

mse,scc,rmse,mae,mape = [],[],[],[],[]

def perf(fold_dir):
    lines = open(fold_dir + '/eval.log').readlines()
    for line in lines:
        res = line.strip().split(':')
        if res[0] == 'Mean squared error':
            mse.append(float(res[1]))
        if res[0] == 'Squared correlation coefficient':
            scc.append(float(res[1]))
        if res[0] == 'RMSE':
            rmse.append(float(res[1]))
        if res[0] == 'MAE':
            mae.append(float(res[1]))
        if res[0] == 'MAPE':
            mape.append(float(res[1].split('%')[0]))

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Usage: %s results dir' % sys.argv[0],file=sys.stderr)
        exit(1)
    klogdir = sys.argv[1]+'/'
    n=0
    for fold in os.listdir(klogdir):
        try:
            perf(klogdir + fold)
            n+=1
        except Exception as Err:
            #print('%s --- ignored' % Err,file=sys.stderr)
            pass
    print('Found', n, 'folds')
    for (measure) in ['mse', 'scc', 'rmse', 'mae', 'mape']:
        print('%s: %.2f $\pm$ %.2f' % ( measure, np.mean(eval(measure)), np.std(eval(measure)) ) )
