/// -*- c++ -*-
/// @file   CaseWrapper.cc
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Wrap around various representations of sparse vectors

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

#include "CaseWrapper.h"
#include "Utility.h"

#include <sys/resource.h>

void pmem() {
  struct rusage r_usage;

  if (getrusage(RUSAGE_SELF, &r_usage)) {
    throw std::out_of_range("getrusage()");
  }
  std::cout << "max_resident, = "
            << r_usage.ru_maxrss;
  std::cout << "  statics, globals, and new/malloc = "
            << r_usage.ru_idrss
            << std::endl;
}

static unsigned malloced = 0;
void malloced_add(unsigned n)
{
  pmem();
  malloced += n;
  std::cout << "++ malloced " << malloced << std::endl;
}
void malloced_sub(unsigned n)
{
  malloced -= n;
  std::cout << "-- malloced " << malloced << std::endl;
  pmem();
}

std::ostream& 
operator<<(std::ostream &os, const CaseWrapper &c)
{
  c.write(os);
  return os;
}

CaseWrapper* new_wrapper(const std::string& mtype) {
  if (mtype=="lb")
    return new LB_CaseWrapper();
  else if (mtype=="libsvm")
    return new LibSVM_CaseWrapper();
  else if (mtype=="external")
    return new LB_CaseWrapper();
  else
    KLOG_THROW("Invalid model type >" << mtype << "< in new_wrapper");
  return NULL;
}
