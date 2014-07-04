# kLog

kLog is a logical and relational language for kernel-based learning allowing to specify learning problems at a high level in a declarative way.
It is implemented as a YAP Prolog module and includes special predicates written in C/C++ and interfaced to Prolog.

The official kLog page is

[http://klog.dinfo.unifi.it](http://klog.dinfo.unifi.it)

# Installation

Install [YAP Prolog](http://www.dcc.fc.up.pt/~vsc/Yap/) first. Version 6.2.2 or later is required.
In order to build kLog, the YAP development files must be properly installed. Tabling should be enabled.
We use the following to build Yap:
```bash
cd yap-6.2.2
mkdir $ARCH
cd $ARCH
../configure --with-readline=no --enable-chr \
--enable-tabling --enable-clpqr --prefix=~/local
make
make install
```
[AUCCalculator](http://mark.goadrich.com/programs/AUC/) is included to perform ROC analysis. A Java runtime environment is required to run AUCCalculator.

kLog can be installed anywhere in your system.
In order to compile kLog's shared library, open your terminal, cd to `src/C` and type `make`.
The YAP include directory should be automatically detected, if not edit by hand the variable `$YAP_INCLUDE` in the `Makefile`.
Finally, you have to inform YAP of kLog by adding the following to `$HOME/.yaprc` (create the file if it does not exist):

```prolog
:- add_to_path('/directory/where/kLog/is/installed/src').
```

To use kLog in your script include at the beginning:
```prolog
:- use_module(klog).
```

The documentation gives you more details.

kLog has been only tested on Linux and MacOS X. Most likely the above procedure also works on Windows under cygwin but we have not tested it.
