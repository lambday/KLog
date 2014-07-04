/// -*- c++ -*-
/// @file   Hash.cc
/// @author Fabrizio Costa <costa@informatik.uni-freiburg.de>
/// @brief  Hashing utilities for the NSPDK

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

#include "Hash.h"

unsigned RSHash(const string& aString) {
  unsigned int b    = 378551;
  unsigned int a    = 63689;
  unsigned int hash = 0;
  for(std::size_t i = 0; i < aString.length(); i++){
    hash = hash * a + aString[i];
    a    = a * b;
  }  
  return hash;
}

unsigned RSHash(const vector<unsigned>& aV) {
  unsigned int b    = 378551;
  unsigned int a    = 63689;
  unsigned int hash = 0;
  for(std::size_t i = 0; i < aV.size(); i++){
    hash = hash * a + aV[i];
    a    = a * b;
  }  
  return hash;
}

unsigned APHash(const string& aString)  {
  unsigned int hash = 0xAAAAAAAA;
  for(std::size_t i = 0; i < aString.length(); i++){
    hash ^= ((i & 1) == 0) ? (  (hash <<  7) ^ aString[i] * (hash >> 3)) :
      (~(((hash << 11) + aString[i] ) ^ (hash >> 5)));
  }
  return hash;
}

unsigned APHash(const vector<unsigned>& aV)  {
  unsigned int hash = 0xAAAAAAAA;
  for(std::size_t i = 0; i < aV.size(); i++){
    hash ^= ((i & 1) == 0) ? (  (hash <<  7) ^ aV[i] * (hash >> 3)) :
      (~(((hash << 11) + aV[i] ) ^ (hash >> 5)));
  }
  return hash;
}

unsigned HashFunc(const string& aString, unsigned aBitMask){//NOTE: extract the least significant bits from the hash
  return  APHash(aString) & aBitMask;
}

unsigned HashFunc(const vector<unsigned>& aList, unsigned aBitMask){
  return  APHash(aList) & aBitMask;
}

