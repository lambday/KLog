/// -*- c++ -*-
/// @file   Histogram.h
/// @author Fabrizio Costa <costa@informatik.uni-freiburg.de>
/// @brief  Histogram for the NSPDK

// Copyright (C) 2013-  Fabrizio Costa, Kurt De Grave, Luc De Raedt, Paolo Frasconi

// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.

// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

#ifndef HISTOGRAM_H
#define HISTOGRAM_H

using namespace std;

class HistogramClass {
    friend ostream& operator<<(ostream& out, const HistogramClass& aHC);
public:
    HistogramClass();
    HistogramClass(const HistogramClass& aH);
    void Insert(unsigned aValue);
    void Insert(unsigned aBin, double aValue);
    void Add(const HistogramClass& aH);
    ostream& Output(ostream& out)const;
    unsigned Size()const;
public:
    map<unsigned, double> mHistogram;
};

//---------------------------------------------------------------------------------

class SecondOrderHistogramClass {
    friend ostream& operator<<(ostream& out, const SecondOrderHistogramClass& aSOH);
public:
    SecondOrderHistogramClass();
    SecondOrderHistogramClass(const SecondOrderHistogramClass& aSOH);
    void Insert(unsigned aSecondOrderBin, unsigned aValue);
    void Insert(unsigned aSecondOrderBin, unsigned aBin, double aValue);
    void Add(const SecondOrderHistogramClass& aSOH);
    void Add(unsigned aSecondOrderBin, const HistogramClass& aH);
    ostream& Output(ostream& out)const;
    unsigned Size()const;
public:
    map<unsigned, HistogramClass> mSecondOrderHistogram;
};

//---------------------------------------------------------------------------------

class ThirdOrderHistogramClass {
    friend ostream& operator<<(ostream& out, const ThirdOrderHistogramClass& aTOH);
public:
    ThirdOrderHistogramClass();
    ThirdOrderHistogramClass(const ThirdOrderHistogramClass& aSOH);
    void Insert(unsigned aThirdOrderBin, unsigned aSecondOrderBin, unsigned aValue);
    void Insert(unsigned aThirdOrderBin, unsigned aSecondOrderBin, unsigned aBin, double aValue);
    void Add(const ThirdOrderHistogramClass& aTOH);
    void Add(unsigned aThirdOrderBin, const SecondOrderHistogramClass& aSOH);
    ostream& Output(ostream& out)const;
    unsigned Size()const;
public:
    map<unsigned, SecondOrderHistogramClass> mThirdOrderHistogram;
};

#endif
