/// -*- c++ -*-
/// @file   Utility.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Misc utils

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

#ifndef UTILITY_H
#define UTILITY_H

#include <iostream>
#include <sstream>
#include <fstream>
#include <cassert>
#include <vector>
#include <stdexcept>
#include <map>
#include <set>
#include <queue>
#include <cmath>
#include <algorithm>
#include <iterator>
#include <cstdio>
#include <cassert>
#include <ctime>
#include <iomanip>
#include <limits>

const std::string NOC = "[0m";
const std::string WHITE = "[1;37m";
const std::string BLACK = "[0;30m";
const std::string BLUE = "[0;34m";
const std::string LBLUE = "[1;34m";
const std::string GREEN = "[0;32m";
const std::string LGREEN = "[1;32m";
const std::string CYAN = "[0;36m";
const std::string LCYAN = "[1;36m";
const std::string RED = "[0;31m";
const std::string LRED = "[1;31m";
const std::string PURPLE = "[0;35m";
const std::string LPURPLE = "[1;35m";
const std::string YELLOW = "[0;33m";
const std::string LYELLOW = "[1;33m";


#define KLOG_THROW(info) {std::stringstream oss;                \
    oss << std::endl << LPURPLE << info << NOC << std::endl     \
        << "function: " << GREEN << __PRETTY_FUNCTION__ << NOC  \
        << " in " << GREEN << __FILE__ << NOC << ":"            \
        << LCYAN << __LINE__ << NOC << std::endl;               \
    throw std::runtime_error(oss.str());}

template <class OutType, class InType>
OutType stream_cast(const InType & t) {
    std::stringstream ss;
    ss << t; // first insert value to stream
    OutType result; // value will be converted to OutType
    ss >> result; // write value to result
    return result;
}

/** 
 * check that a std container is sorted ascending
 * 
 * @param v  <a generic container>
 */
template <typename T> void check_sorted_ascending(const T& v) {
    typename T::const_iterator i0 = v.begin();
    if (i0 == v.end()) return;
    typename T::const_iterator i1 = i0;
    i1++;
    while (i1 != v.end())
        if (*(i1++) < *(i0++)) {
            std::stringstream oss;
            oss << "a container was expected to be sorted\n"
                    << "failed comparison: " << *(--i1) << " < "
                    << *(--i0);
            throw std::logic_error(oss.str());
        }
}

/** 
 * Split a string on whitespaces
 * 
 * @param s  <a string>
 */

inline std::vector<std::string> split(const std::string s) {
    std::istringstream iss(s);
    std::vector<std::string> items;
    copy(std::istream_iterator<std::string>(iss),
            std::istream_iterator<std::string>(),
            std::back_inserter<std::vector<std::string> >(items));
    return items;
}

#endif
