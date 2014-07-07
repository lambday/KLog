/// -*- c++ -*-
/// @file   FeatureGeneratorPool.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Pool of feature generators

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


#ifndef FEATUREGENERATORPOOL_H_
#define FEATUREGENERATORPOOL_H_

#include <vector>
#include <map>
#include <iostream>
#include <fstream>
#include "FeatureGenerator.h"
#include "NSPDK_FeatureGenerator.h"
#include "Utility.h"

/** This singleton is a pool of feature generators. Its internal
dictionary maps strings (as seen by prolog) into FeatureGenerator
pointers allowing kLog code to refer to different feature generators
using atoms as identifiers. Messages are sent to this class from
prolog via the corresponding c_xxx functions defined in
c_interface.cpp. See elsewhere the documentation of each particular
method. */
class FeatureGeneratorPool {
private:
    typedef std::map<std::string, FeatureGenerator*> Map;
    Map feature_generator_pool;

    FeatureGeneratorPool() {
    };
    FeatureGeneratorPool(FeatureGeneratorPool const& rhs);
    FeatureGeneratorPool& operator=(FeatureGeneratorPool const& rhs);

public:

    static FeatureGeneratorPool& get_instance() {
        static FeatureGeneratorPool instance;
        return instance;
    }

    FeatureGenerator* get_feature_generator(const std::string& gid) {
        Map::iterator found = feature_generator_pool.find(gid);
        if (found == feature_generator_pool.end()) {
            KLOG_THROW("Invalid feature-generator identifier >" << gid << "<");
        }
        return (*found).second;
    }

    void new_feature_generator(const std::string& gid, const std::string& gtype) {
        Map::iterator found = feature_generator_pool.find(gid);
        if (found != feature_generator_pool.end()) {
            //      throw std::out_of_range("Duplicate feature_generator identifier in new_feature_generator()");
            std::cout << "Warning: overwriting feature_generator " << gid << std::endl;
            delete found->second;
        }
        FeatureGenerator* g;
        if (gtype == "nspdk") {
            g = new NSPDK_FeatureGenerator(gid);
        }
        else if (gtype == "mnspdk") {
            g = new MNSPDK_FeatureGenerator(gid);
        }
        else if (gtype == "rnspdk") {
            g = new RNSPDK_FeatureGenerator(gid);
        }
        else if (gtype == "anspdk") {
            g = new ANSPDK_FeatureGenerator(gid);
        }
        else if (gtype == "alnspdk") {
            g = new ALNSPDK_FeatureGenerator(gid);
        }
        else if (gtype == "nspdk3d") {
            g = new NSPDK3D_FeatureGenerator(gid);
        }
        else {
            KLOG_THROW("Invalid feature-generator type >" << gtype << "<");
        }
        feature_generator_pool[gid] = g;
    }

    void delete_feature_generator(const std::string& gid) {
        Map::iterator found = feature_generator_pool.find(gid);
        if (found != feature_generator_pool.end()) {
            delete (*found).second;
            feature_generator_pool.erase(found);
        } else {
            KLOG_THROW("Invalid feature-generator identifier >" << gid << "<");
        }
    }
};

extern FeatureGeneratorPool& The_FeatureGeneratorPool;

#endif /* FEATUREGENERATORPOOL_H_ */
