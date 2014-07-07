/// -*- c++ -*-
/// @file   ModelPool.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Pool of models used by kLog

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

#ifndef MODELPOOL_H_
#define MODELPOOL_H_

#include <vector>
#include <map>
#include <iostream>
#include <fstream>
#include "LB_ScalarModel.h"
#include "LibSVM_Model.h"
#include "External_Model.h"
#include "CaseWrapper.h"
#include "Utility.h"

/** This singleton is a pool of models. Its internal dictionary maps
strings (as seen by prolog) into Model pointers allowing kLog code to
refer to different models using atoms as identifiers. Messages are
sent to this class from prolog via the corresponding c_xxx functions
defined in c_interface.cpp. See elsewhere the documentation of each
particular method. 

Valid models are currently svm_sgd, lr_sgd, adaboost_sgd, ols_sgd. See
definitions in Loss.h

 */


class ModelPool {
private:
    typedef std::map<std::string, Model*> Map;
    Map model_pool;

    ModelPool() {
    };
    ModelPool(ModelPool const& rhs);
    ModelPool& operator=(ModelPool const& rhs);

public:

    static ModelPool& get_instance() {
        static ModelPool instance;
        return instance;
    }

    Model* get_model(const std::string& mid) {
        Map::iterator found = model_pool.find(mid);
        if (found == model_pool.end()) {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
        return (*found).second;
    }

    void new_model(const std::string& mid, const std::string& mtype) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            //      throw std::out_of_range("Duplicate model identifier in new_model()");
            std::cout << "Warning: overwriting model " << mid << std::endl;
            delete found->second;
        }
        Model* m;
        if (mtype == "svm_sgd") {
            m = new LB_ScalarModel<HingeLoss, BinaryClassifierReporter>(mid);
        } else if (mtype == "lr_sgd") {
            m = new LB_ScalarModel<SoftHingeLoss, BinaryClassifierReporter>(mid);
        } else if (mtype == "adaboost_sgd") {
            m = new LB_ScalarModel<ExponentialLoss, BinaryClassifierReporter>(mid);
        } else if (mtype == "ols_sgd") {
            m = new LB_ScalarModel<SquareLoss, RegressionReporter>(mid);
        } else if (mtype == "svr_sgd") {
            m = new LB_ScalarModel<EpsilonInsensitiveLoss, RegressionReporter>(mid);
        } else if (mtype == "libsvm_c_svc") {
            m = new Libsvm_Model<BinaryClassifierReporter>(mid, 0);
        } else if (mtype == "libsvm_c_svc_mc") {
            m = new Libsvm_Model<MulticlassClassifierReporter>(mid, 0);
        } else if (mtype == "libsvm_nu_svc") {
            m = new Libsvm_Model<BinaryClassifierReporter>(mid, 1);
        } else if (mtype == "libsvm_eps_svr") {
            m = new Libsvm_Model<RegressionReporter>(mid, 3);
        } else if (mtype == "libsvm_nu_svr") {
            m = new Libsvm_Model<RegressionReporter>(mid, 4);
        } else if (mtype == "external_c") {
            m = new External_Model<ExternalBinaryClassifierReporter>(mid);
        } else if (mtype == "external_mc") {
            m = new External_Model<ExternalMulticlassClassifierReporter>(mid);
        } else {
            KLOG_THROW("Unknown model type >" << mtype << "<");
        }
        model_pool[mid] = m;
    }

    void delete_model(const std::string& mid) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            delete (*found).second;
            model_pool.erase(found);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    std::string get_model_type(const std::string& mid) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            return (*found).second->get_model_type();
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void write_model(const std::string& mid) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            std::cout << (*found).second->str();
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void load_model(const std::string& mid, const std::string& filename) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            found->second->load(filename);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void save_model(const std::string& mid, const std::string& filename) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            // std::ofstream os(filename.c_str());
            // os << (*found).second->str();
            // os.close();
            found->second->save(filename);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    bool check_model_ability(const std::string& mid, const std::string& ability) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            return found->second->check_ability(ability);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void train_model(const std::string& mid, const std::vector<std::string>& interpretation_ids) {
        // std::cout << "C++ train_model on interpretation_ids:";
        // for (unsigned i=0; i<interpretation_ids.size(); ++i)
        //   std::cout << " " << interpretation_ids[i];
        // std::cout << std::endl;
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            m->train(interpretation_ids);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void test_dataset(const std::string& mid, const std::vector<std::string>& interpretation_ids, bool newdata) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            m->test_dataset(interpretation_ids, newdata);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void set_model_wd(const std::string& mid, const std::string& wd) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            m->set_model_wd(wd);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void reset_reporter(const std::string& mid, const std::string& reporter, const std::string& desc = "") {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            m->reset_reporter(reporter, desc);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void report(const std::string& mid, std::ostream& os, const std::string& reporter) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            m->report(os, reporter);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    void save_predictions(const std::string& mid, const std::string& dir, const std::string& reporter) {
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            m->save_predictions(dir, reporter);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
    }

    double get_prediction(const std::string& mid, const std::string& caseid) {
        double margin = 0;
        Map::iterator found = model_pool.find(mid);
        if (found != model_pool.end()) {
            Model* m = found->second;
            margin = m->get_prediction(caseid);
        } else {
            KLOG_THROW("Invalid model identifier >" << mid << "<");
        }
        return margin;
    }

    void print_model_ids(void) {
        for (Map::iterator i = model_pool.begin(); i != model_pool.end(); ++i)
            std::cout << i->first << std::endl;
    }
};

ModelPool& The_ModelPool = ModelPool::get_instance();


#endif /* MODELPOOL_H_ */
