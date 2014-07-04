#!/usr/bin/env python
from __future__ import print_function
import numpy as np
import os,sys,subprocess

experiments = {}
ansi = { 'NOC': "[0m",  'WHITE': "[1;37m",  'BLACK': "[0;30m",  'BLUE': "[0;34m",  'LBLUE': "[1;34m",  'GREEN':  "[0;32m",  'LGREEN': "[1;32m",  'CYAN': "[0;36m",  'LCYAN': "[1;36m",  'RED': "[0;31m",  'LRED': "[1;31m",  'PURPLE': "[0;35m",  'LPURPLE': "[1;35m",  'YELLOW': "[0;33m",  'LYELLOW': "[1;33m"}

def cmp(klogdir, a, b, par_dict_a, par_dict_b):
    args = ['diff', klogdir+a+'/hash_key.log', klogdir+b+'/hash_key.log']
    lines = subprocess.Popen(args,stdout=subprocess.PIPE).stdout.readlines()
    for line in lines:
        res = line.strip().split()
        if (res[0]=='>' or res[0]=='<') and len(res)>2:
            param = res[1]
            if '::' in param and '[' in res[2] and ']' in res[2]:
                val = res[2].split('[')[1].split(']')[0]
                if res[0]=='<':
                    par_dict_a[param] = val
                else:
                    par_dict_b[param] = val


def fold_testset(fold_dir,fold_dict):
    lines = open(fold_dir + '/test.txt').readlines()
    l = lines[0].strip()[1:-1]
    fold_dict['*testset'] = '[' + l[:14] + ']' + max(0,14-len(l[:14]))*' '

def eval_fold_regression(fold_dir,fold_dict):
    lines = open(fold_dir + '/eval.log').readlines()
    for line in lines:
        res = line.strip().split(':')
        if res[0] == 'Mean squared error':
            fold_dict['mse']=float(res[1])
        if res[0] == 'Squared correlation coefficient':
            fold_dict['scc']=float(res[1])
        if res[0] == 'RMSE':
            fold_dict['rmse']=float(res[1])
        if res[0] == 'MAE':
            fold_dict['mae']=float(res[1])
        if res[0] == 'MAPE':
            fold_dict['mape']=float(res[1].split('%')[0])
    fold_testset(fold_dir,fold_dict)

def eval_fold_binary(fold_dir,fold_dict):
    lines = open(fold_dir + '/eval.log').readlines()
    for line in lines:
        res = line.strip().split(':')
        if res[0] == 'Error':
            fold_dict['error']=float(res[1][:-1])
        if res[0] == 'Precision':
            fold_dict['precision']=float(res[1][:-1])
        if res[0] == 'Recall':
            fold_dict['recall']=float(res[1][:-1])
        if res[0] == 'F1-measure':
            fold_dict['f1']=float(res[1][:-1])
    args = ['tail', '-2', fold_dir+'/auc.log']
    # lines = open(fold_dir + '/auc.log').readlines()
    lines = subprocess.Popen(args,stdout=subprocess.PIPE).stdout.readlines()
    for line in lines:
        res = line.strip().split(' is ')
        if res[0] == 'Area Under the Curve for Precision - Recall':
            fold_dict['auprc']=float(res[1])
        if res[0] == 'Area Under the Curve for ROC':
            fold_dict['auroc']=float(res[1])
    fold_testset(fold_dir,fold_dict)


def eval_exp(exp_dir,exp_dict, mode):
    for fold_dir in os.listdir(exp_dir):
        exp_dict[fold_dir] = {}
        try:
            if mode=='binary':
                eval_fold_binary(exp_dir + '/' + fold_dir,exp_dict[fold_dir])
            if mode=='regression':
                eval_fold_regression(exp_dir + '/' + fold_dir,exp_dict[fold_dir])
        except IOError as Err:
            # print('eval_exp ', exp_dir, Err)
            del exp_dict[fold_dir]

def eval_all(klogdir,mode):
    for exp_dir in os.listdir(klogdir):
        try:
            if exp_dir not in experiments:
                experiments[exp_dir] = {}
                experiments[exp_dir]['params'] = {}
            eval_exp(klogdir+exp_dir,experiments[exp_dir],mode)
        except IOError as Err:
            # print('eval_all ', klogdir, Err)
            del experiments[exp_dir]

def print_params(params,what):
    print("\t".join(["%s\t%s" % (param, value) for param,value in sorted(params.items())]),end="")
    if what=='long':
        print()
    else:
        print('\t',end="")
        
def report(what):
    for e in experiments:
        if what=='long':
            print(ansi['LGREEN']+e+ansi['NOC'])
        measures = {}
        print_params(experiments[e]['params'],what)
        for fold in iter(sorted(experiments[e])):
            if fold != 'params':
                if what=='long':
                    # print(fold,experiments[e][fold])
                    print(ansi['WHITE']+fold+ansi['NOC'],end=' ')
                    print(ansi['CYAN']+experiments[e][fold]['*testset']+ansi['NOC'],end='')
                for measure,value in experiments[e][fold].items():
                    if measure[0]!='*':
                        if measure not in measures:
                            measures[measure] = []
                        if what=='long':
                            print(' %s: %.2f' % (measure,value),end='')
                        measures[measure].append(value)
                if what=='long':
                    print()
        for measure,vals in measures.items():
            if what=='long' or what==measure:
                print('%-12s %.2f $\pm$ %.2f' % ( measure, np.mean(vals), np.std(vals) ) )


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('Usage: %s results dir <binary|regression> [measure]' % sys.argv[0],file=sys.stderr)
        print('where measure is one of f1,recall,precision,auprc,auroc,error')
        exit(1)
    klogdir = sys.argv[1]+'/'
    eval_all(klogdir,sys.argv[2])
    n=0
    for a in os.listdir(klogdir):
        for b in os.listdir(klogdir):
            if a!=b:
                try:
                    cmp(klogdir, a, b, experiments[a]['params'],experiments[b]['params'])
                    n+=1
                except Exception as Err:
                    print('%s --- ignored' % Err,file=sys.stderr)
                    pass
    print('Found', n, 'pairs of experiments')
    if len(sys.argv) > 3:
        report(sys.argv[3])
    else:
        report('long')

