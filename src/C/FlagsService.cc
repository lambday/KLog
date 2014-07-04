/// -*- c++ -*-
/// @file   FlagsService.cc
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Service for enriching C++ objects with flags

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


#include "FlagsService.h"

FlagsServiceClient::FlagsServiceClient(const std::string& id_for_flags_service) {
  this->id_for_flags_service = id_for_flags_service;
  The_FlagsService.register_flags_service_client(this);
}

FlagsServiceClient::~FlagsServiceClient() {
  for (FMap::const_iterator i=flags_traits.begin(); i!=flags_traits.end(); ++i)
    delete i->second;
  The_FlagsService.unregister_flags_service_client(this);
}
