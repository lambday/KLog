/// -*- c++ -*-
/// @file   Model.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Abstract model

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

#ifndef MODEL_H_
#define MODEL_H_

#include <string>
#include <iostream>
#include <sstream>
#include "Dataset.h"

class Model {
protected:
    std::string model_id;
    std::string model_type;
    std::string working_directory;
public:

    Model(std::string model_id = "void_model") {
        this->model_id = model_id;
        model_type = "null_model";
        working_directory = ".";
    };

    virtual ~Model() {
    };

    virtual std::string str(void) {
        std::stringstream oss;
        oss << "Model: >" << model_id << "<" << std::endl;
        return oss.str();
    }

    std::string get_model_type(void) const {
        return model_type;
    }

    void set_model_wd(const std::string& wd) {
        working_directory = wd;
    }
    virtual void load(const std::string filename) = 0;
    virtual void save(const std::string filename) = 0;
    virtual bool check_ability(const std::string ability) = 0;
    virtual void train(const std::vector<std::string>& interpretation_ids) = 0;
    virtual void test_dataset(const std::vector<std::string>& interpretation_ids, bool newdata) = 0;
    virtual void reset_reporter(const std::string& r, const std::string& desc) = 0;
    virtual void report(std::ostream& os, const std::string& r) = 0;
    virtual void save_predictions(const std::string& dir, const std::string& r) = 0;
    virtual double get_prediction(const std::string& caseid) = 0;
};

#endif /* MODEL_H_ */
