/// -*- c++ -*-
/// @file   shasum.h
/// @author Fabrizio Costa <costa@informatik.uni-freiburg.de>
/// @brief  Feature hashing for the NSPDK

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

#ifndef _SHASUM_H
#define _SHASUM_H 1

#include <iostream>
#include <stdexcept>
#include <string>
#include <sstream>
#include "sha1.h"

 /**
  * User-friendly interface to compute shasum of a string
  * 
  * @param s <input string>
  * @return shasum of input string
  */
inline std::string shasum(const std::string& s) {
  std::string hashvalue;
  SHA1Context sha;  // SHA-1 context
  SHA1Reset(&sha);
  unsigned char* c = (unsigned char*)s.c_str();
  while (*c) {
    SHA1Input(&sha, c, 1);
    c++;
  }
  if (!SHA1Result(&sha))
    throw std::runtime_error("shasum: could not compute shasum for string " + s);
  std::ostringstream os;
  for (int i=0; i<5; ++i )
    os << std::hex << sha.Message_Digest[i];
  return os.str();
}

template<typename Iterable>
std::string shasum_i(const Iterable& c) {
  std::ostringstream os;
  for (typename Iterable::const_iterator i = c.begin(); i!=c.end(); ++i)
    os << *i;
  return shasum(os.str());
}

#endif // _SHASUM_H
