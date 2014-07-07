/// -*- c++ -*-
/// @file   Hash.h
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

#ifndef HASH_H
#define HASH_H

#include "Utility.h"
#include <limits>

using namespace std;

unsigned HashFunc(const string& aString, unsigned aBitMask = (std::numeric_limits<unsigned>::max() >> 1));
unsigned HashFunc(const vector<unsigned>& aList, unsigned aBitMask = (std::numeric_limits<unsigned>::max() >> 1));

#endif
