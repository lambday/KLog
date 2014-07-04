/// -*- c++ -*-
/// @file   Histogram.cc
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

#include "Utility.h"
#include "Histogram.h"

//---------------------------------------------------------------------------------
ostream& operator<<(ostream& out, const HistogramClass& aHC){return aHC.Output(out);}
HistogramClass::HistogramClass(){}
HistogramClass::HistogramClass(const HistogramClass& aH):mHistogram(aH.mHistogram){}
void HistogramClass::Insert(unsigned aValue){
    if (mHistogram.count(aValue)==0) mHistogram[aValue]=1;
    else mHistogram[aValue]++;
  }
void HistogramClass::Insert(unsigned aBin, double aValue){
    if (mHistogram.count(aBin)==0) mHistogram[aBin]=aValue;
    else mHistogram[aBin]+=aValue;
  }
void HistogramClass::Add(const HistogramClass& aH){
    for(map<unsigned,double>::const_iterator it=aH.mHistogram.begin();it!=aH.mHistogram.end();++it){
      unsigned bin=it->first;
      double value=it->second;
      Insert(bin,value);
    }
  }
ostream& HistogramClass::Output(ostream& out)const{
    for(map<unsigned,double>::const_iterator it=mHistogram.begin();it!=mHistogram.end();++it)
      out<<"      "<<it->first<<" : "<<it->second<<" ";
    return out;
  }
unsigned HistogramClass::Size()const{return mHistogram.size();}

//---------------------------------------------------------------------------------
ostream& operator<<(ostream& out, const SecondOrderHistogramClass& aSOH){return aSOH.Output(out);}
SecondOrderHistogramClass::SecondOrderHistogramClass(){}
SecondOrderHistogramClass::SecondOrderHistogramClass(const SecondOrderHistogramClass& aSOH):mSecondOrderHistogram(aSOH.mSecondOrderHistogram){}
void SecondOrderHistogramClass::Insert(unsigned aSecondOrderBin, unsigned aValue){
    if (mSecondOrderHistogram.count(aSecondOrderBin)==0) mSecondOrderHistogram[aSecondOrderBin]=HistogramClass();
    mSecondOrderHistogram[aSecondOrderBin].Insert(aValue);
  }
void SecondOrderHistogramClass::Insert(unsigned aSecondOrderBin, unsigned aBin, double aValue){
    if (mSecondOrderHistogram.count(aSecondOrderBin)==0) mSecondOrderHistogram[aSecondOrderBin]=HistogramClass();
    mSecondOrderHistogram[aSecondOrderBin].Insert(aBin,aValue);
  }
void SecondOrderHistogramClass::Add(const SecondOrderHistogramClass& aSOH){
    for(map<unsigned,HistogramClass>::const_iterator it=aSOH.mSecondOrderHistogram.begin();it!=aSOH.mSecondOrderHistogram.end();++it){
      unsigned second_order_bin=it->first;
      const HistogramClass& histogram=it->second;
      mSecondOrderHistogram[second_order_bin].Add(histogram);
    }
  }
void SecondOrderHistogramClass::Add(unsigned aSecondOrderBin,const HistogramClass& aH){
    mSecondOrderHistogram[aSecondOrderBin].Add(aH);
  }
ostream& SecondOrderHistogramClass::Output(ostream& out)const{
    for(map<unsigned,HistogramClass>::const_iterator it=mSecondOrderHistogram.begin();it!=mSecondOrderHistogram.end();++it)
      out<<"    "<<it->first<<" : "<<endl<<it->second<<endl<<endl;
    return out;
  }
unsigned SecondOrderHistogramClass::Size()const{return mSecondOrderHistogram.size();}

//---------------------------------------------------------------------------------
ostream& operator<<(ostream& out, const ThirdOrderHistogramClass& aTOH){return aTOH.Output(out);}
ThirdOrderHistogramClass::ThirdOrderHistogramClass(){}
ThirdOrderHistogramClass::ThirdOrderHistogramClass(const ThirdOrderHistogramClass& aSOH):mThirdOrderHistogram(aSOH.mThirdOrderHistogram){}
void ThirdOrderHistogramClass::Insert(unsigned aThirdOrderBin, unsigned aSecondOrderBin, unsigned aValue){
    if (mThirdOrderHistogram.count(aThirdOrderBin)==0) mThirdOrderHistogram[aThirdOrderBin]=SecondOrderHistogramClass();
    mThirdOrderHistogram[aThirdOrderBin].Insert(aSecondOrderBin,aValue);
  }
void ThirdOrderHistogramClass::Insert(unsigned aThirdOrderBin, unsigned aSecondOrderBin, unsigned aBin, double aValue){
    if (mThirdOrderHistogram.count(aThirdOrderBin)==0) mThirdOrderHistogram[aThirdOrderBin]=SecondOrderHistogramClass();
    mThirdOrderHistogram[aThirdOrderBin].Insert(aSecondOrderBin,aBin,aValue);
  }
void ThirdOrderHistogramClass::Add(const ThirdOrderHistogramClass& aTOH){
    for(map<unsigned,SecondOrderHistogramClass>::const_iterator it=aTOH.mThirdOrderHistogram.begin();it!=aTOH.mThirdOrderHistogram.end();++it){
      unsigned third_order_bin=it->first;
      const SecondOrderHistogramClass& so_histogram=it->second;
      mThirdOrderHistogram[third_order_bin].Add(so_histogram);
    }
  }
void ThirdOrderHistogramClass::Add(unsigned aThirdOrderBin,const SecondOrderHistogramClass& aSOH){
    mThirdOrderHistogram[aThirdOrderBin].Add(aSOH);
  }
ostream& ThirdOrderHistogramClass::Output(ostream& out)const{
    for(map<unsigned,SecondOrderHistogramClass>::const_iterator it=mThirdOrderHistogram.begin();it!=mThirdOrderHistogram.end();++it)
      out<<"  "<<it->first<<" : "<<endl<<it->second<<endl<<endl;
    return out;
  }
unsigned ThirdOrderHistogramClass::Size()const{return mThirdOrderHistogram.size();}
