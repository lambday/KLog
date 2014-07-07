/// -*- c++ -*-
/// @file   kLogMaster.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Master flags singleton

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


#ifndef KLOGMASTER_H_
#define KLOGMASTER_H_

#include <string>
#include "FlagsService.h"

class kLogMaster : public FlagsServiceClient {
private:
    unsigned verbosity;
    std::string install_directory;
    std::string save_subgraphs_directory;
public:

    kLogMaster() : FlagsServiceClient("klog_master") {
        new_flag(&verbosity, "verbosity",
                "(0..5)\nVerbosity level: 0=silent, 1=errors, 2=warnings, 3=progress, 4=debug, 5=detailed debug");
        new_flag(&install_directory, "install_directory",
                "klog installation directory");
        new_flag(&save_subgraphs_directory, "save_subgraphs_directory",
                "where GML subgraphs associated to cases are saved (e.g. for use with other kernels);\nIf empty then subsgraphs are not saved");
    }

    unsigned get_verbosity(void) const {
        return verbosity;
    }

    const std::string& get_install_directory(void) const {
        return install_directory;
    }

    const std::string& get_save_subgraphs_directory(void) const {
        return save_subgraphs_directory;
    }
};

extern kLogMaster The_Master;
// FIXME: singleton??

#endif // KLOGMASTER_H_
