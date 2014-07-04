## Makefile for the kLog shared library

## Copyright (C) 2013-  Fabrizio Costa, Kurt De Grave, Luc De Raedt, Paolo Frasconi

## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 2.1 of the License, or (at your option) any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.

## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

## Tested with GNU Make 3.81


## YAP-Prolog is required to compile kLog. If the automated search
## below does not work in your system because the yap binary is
## installed in a non standard way, enter here the path to the YAP
## include directory
YAP_BIN = /usr/local/bin/ #$(shell which yap)
ifeq ($(YAP_BIN),)
    P=$(shell printenv PATH)
    $(error Fatal: YAP-prolog not found in ${P}. Edit Makefile and set YAP_INCLUDE manually)
endif
#YAP_INCLUDE = $(shell dirname $(YAP_BIN))/../include
# YAP_INCLUDE=/Users/paolo/Documents/Research/Software/local/include
YAP_INCLUDE= /usr/local/include/

## Compiler optimization flags
OPT_RELEASE_FLAGS = -O3 # Note: -O4 fails with clang 2.0 (LLVM 2.9svn)
# OPT_FLAGS = -g -finstrument-functions -m32
OPT_DEBUG_FLAGS = -g

## We have tested MacOS 10.6 (Kernel Version 10.8.0, x86_64), Debian
## squeeze (2.6.38-2-amd64, x86_64), Ubuntu 11.04 (2.6.38-8-generic,
## x86_64).

UNAME = $(shell uname)
ifeq ($(UNAME),Darwin)
    ## Under MacOS, clang is preferred if installed
    CXX = $(shell which clang++)
    ifeq ($(CXX),)
        CXX = $(shell which g++)
    endif
    TARGET = c_interface.dylib
    CXXFLAGS = -dynamic -Wall -I$(YAP_INCLUDE)
endif
ifeq ($(UNAME),Linux)
    ## At least under Ubuntu 11.04, clang++ include path is broken...
    CXX = g++
    TARGET = c_interface.so
    CXXFLAGS = -shared -fPIC -Wall -I$(YAP_INCLUDE)
endif
ifeq ($(TARGET),)
    $(error Fatal: OS not recognized. Edit Makefile and fix manually)
endif


NSPDK.h = Dataset.h\
	FlagsService.h\
	CaseWrapper.h\
	BaseGraphClass.h\
	GraphClass.h\
	kLogMaster.h\
	Histogram.h\
	vectors.h\
	wrapper.h\
	FeatureGenerator.h\
	FeatureGeneratorPool.h\
	NSPDK_FeatureGenerator.h\
	Loss.h\
	Hash.h\
	Model.h\
	ModelPool.h\
	LB_ScalarModel.h\
	LibSVM_Model.h\
	timer.h\
	sha1.h
NSPDK.cpp= NSPDK_FeatureGenerator.cc\
	BaseGraphClass.cc\
	CaseWrapper.cc\
	GraphClass.cc\
	FlagsService.cc\
	Histogram.cc\
	vectors.cc\
	timer.cc\
	sha1.cc\
	Hash.cc\
	libsvm/svm.cc

# NSPDK.o = $(patsubst %.cc,%.o,$(NSPDK.cpp))
NSPDK.o = $(patsubst %.c,%.o,$(patsubst %.cc,%.o,$(NSPDK.cpp)))

.PHONY: release
release: CXXFLAGS+=$(OPT_RELEASE_FLAGS)
release: $(TARGET)

.PHONY: debug
debug: CXXFLAGS+=$(OPT_DEBUG_FLAGS)
debug: $(TARGET)

# .PHONY: all
# all: $(TARGET)

# Linux
c_interface.so: c_interface.o $(NSPDK.o)
	$(CXX) -g -shared -Wl,-soname,c_interface.so -o c_interface.so c_interface.o $(NSPDK.o)

# MacOS
c_interface.dylib: c_interface.o $(NSPDK.o)
	$(CXX) -framework CoreFoundation -g -bundle -flat_namespace -undefined suppress  -o c_interface.dylib c_interface.o $(NSPDK.o)

drive: drive.o vectors.o FlagsService.o timer.o libsvm/svm.o
	$(CXX) -g drive.o vectors.o FlagsService.o timer.o libsvm/svm.o -o drive
# g++ -m32 -g -finstrument-functions drive.o vectors.o FlagsService.o timer.o -o drive -lSaturn
drive.o: drive.cpp LB_ScalarModel.h Loss.h ModelPool.h vectors.h FlagsService.h timer.h

testcw.o: CaseWrapper.h vectors.h
testcw: testcw.o vectors.o CaseWrapper.o
	$(CXX) -g testcw.o vectors.o CaseWrapper.o -o testcw

.PHONY: clean
clean:
	rm -f *.so *.o *.dylib libsvm/svm.o

depend: $(NSPDK.cpp) c_interface.cpp
	$(CXX) -w -MM $(CXXFLAGS) $^ > $@
include depend
