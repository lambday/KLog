/// -*- c++ -*-
/// @file   External_Model.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Allow kLog to use an external learner

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


#ifndef EXTERNAL_MODEL_H_
#define EXTERNAL_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <stdlib.h>

#include "timer.h"
#include "Model.h"
#include "FlagsService.h"
#include "Performance.h"
#include "Utility.h"
#include "Dataset.h"
#include "shasum.h"

/** 
 * External model
 */

template<typename PerformanceReporter>
class External_Model : public Model, public FlagsServiceClient {
private:


    std::string train_command; /**< External command for train */
    std::string test_command; /**< External command for test */
    /**
     * train_command uses two special substitutions:
     * $datafile   Name of the file containing training data
     * $modelfile  Name of the file where the model is saved
     * test_command uses three special substitutions:
     * $datafile   Name of the file containing test data
     * $modelfile  Name of the file containing the learned model
     * $outfile    Name of the file containing predicted margins (one line for each case)
     * 
     * svm_light and similar programs already use this interface. In
     * other cases you may have to wrap/modify software to comply with
     * this interface.
     */

    std::string model_file; /**< Created by train, read by test_dataset */
    /** Global performance reporter. Remembers all predictions on the
        entire data set.
     */
    bool save_qid; /**< If true saves a qid for each interpretation */
    static PerformanceReporter global_reporter;
    /** Local performance reporter (for individual folds). */
    PerformanceReporter reporter;

private:

    void replace(std::string& text, const std::string& a, const std::string& b) {
        size_t index = text.find(a, 0);
        if (index == std::string::npos)
            KLOG_THROW("Malformed command string: Substring >" << a
                << "< not found in >" << text << "<");
        text.replace(index, a.length(), b);
    }

    std::string sv_base(void) {
        return working_directory + "/sv";
    }

    void my_system(std::string command) {
        if (system(command.c_str()))
            KLOG_THROW("system call >" << command << "< failed");
    }

    std::string prepare_data_files(const std::vector<std::string>& interpretation_ids) {
        std::string basename = sv_base() + "/" + shasum_i(interpretation_ids);
        // after all reusing is a bad idea..
        // ifstream check((basename+".sv").c_str());
        // if (!check.good()) {
        my_system("mkdir -p " + sv_base());
        The_Dataset.save_as_libsvm_file(basename, interpretation_ids, save_qid);
        std::ofstream int_stream;
        int_stream.open((basename + ".int").c_str(), ios::out);
        for (unsigned i = 0; i < interpretation_ids.size(); ++i)
            int_stream << interpretation_ids[i] << std::endl;
        int_stream.close();
        // } else {
        //   std::cout << "reusing " << basename+".sv" << std::endl;
        // }
        return basename;
    }
public:

    /**
     * Creates a new External_Model model. All parameters are given default values:
     * 
     * @param id <model identifier>
     */
    External_Model(const std::string& id) : Model(id), FlagsServiceClient(id), reporter("Local report") {
        model_type = "external";
        new_flag(&train_command, "train_command", "(string)\nHow to invoke training");
        new_flag(&test_command, "test_command", "(string)\nHow to invoke testing");
        new_flag(&save_qid, "save_qid", "(bool)\nSave qid (one qid for every interpretation))");
        train_command = "echo 'train command not set'";
        test_command = "echo 'test command not set'";
        save_qid = false;
        reporter.reset();
    }

    virtual ~External_Model() {
    }

    /**
     * Train the model
     * @param interpretation_ids <vector of interpretation identifiers>
     */
    void train(const std::vector<std::string>& interpretation_ids) {
        std::string basename = prepare_data_files(interpretation_ids);
        model_file = basename + ".model";
        std::string actual_train_command = train_command;
        replace(actual_train_command, "$datafile", basename + ".sv");
        replace(actual_train_command, "$modelfile", model_file);
        std::cout << LCYAN << actual_train_command << NOC << std::endl;
        my_system(actual_train_command);
    }

    /**
     * Classify a data set
     * @param interpretation_ids <vector of interpretation identifiers>
     */
    void test_dataset(const std::vector<std::string>& interpretation_ids, bool newdata) {
        std::string basename = prepare_data_files(interpretation_ids);
        std::string actual_test_command = test_command;
        replace(actual_test_command, "$datafile", basename + ".sv");
        replace(actual_test_command, "$modelfile", model_file);
        replace(actual_test_command, "$outputfile", basename + ".out");
        std::cout << LCYAN << actual_test_command << NOC << std::endl;
        my_system(actual_test_command);
        if (newdata)
            reporter.reset("Local reporter  (on new data)");
        else
            reporter.reset("Local reporter  (on train data)");
        reporter.read_predictions(basename);
        if (newdata)
            global_reporter.read_predictions(basename);
    }

    /** 
     * Reset a performance reporter
     * @param r <reporter identifier>
     */
    void reset_reporter(const std::string& r, const std::string& desc = "") {
        if (r == "local")
            reporter.reset(desc);
        else if (r == "global")
            global_reporter.reset(desc);
        else
            KLOG_THROW("Invalid reporter ID >" << r << "<");
    }

    /** 
     * Get a report on individually tested cases using a performance
     * reporter
     */
    void report(std::ostream& os, const std::string& r) {
        if (r == "local")
            reporter.report(os);
        else if (r == "global")
            global_reporter.report(os);
        else
            KLOG_THROW("Invalid reporter ID >" << r << "<");
    }

    /** 
     * Save predictions stored in the global performance reporter
     */
    void save_predictions(const std::string& dir, const std::string& r) {
        if (r == "local")
            reporter.save_predictions(dir);
        else if (r == "global")
            global_reporter.save_predictions(dir);
        else
            KLOG_THROW("Invalid reporter ID >" << r << "<");
    }

    /**
     * Serializer.
     * Format is Key: value on each line where Key is the name of a
     * variable in the model.
     * @return <string representing the model, compatible with saved
     *        file format>
     */
    std::string str(void) {
        std::stringstream oss;
        oss << "Model: " << model_id << std::endl;
        oss << "train_command: " << train_command << std::endl;
        oss << "test_command: " << test_command << std::endl;
        return oss.str();
    }

    /**
     * Load a model from a file. The expected format is the same produced by save()
     * @param filename <filename>
     */
    void load(std::string filename) {
        KLOG_THROW("load not supported by external models - load("
                << filename << ")");
    }

    /**
     * Save a model to file. The saved model can be read by load()
     * @param filename <doc>
     */
    void save(std::string filename) {
        KLOG_THROW("save not supported by external models - save("
                << filename << ")");
    }

    bool check_ability(std::string ab) {
        std::cout << "Warning: Required ability " << ab
                << "granted to external model "
                << model_id << std::endl;
        return true;
    }

    double get_prediction(const std::string& caseid) {
        return global_reporter.get_prediction(caseid);
    }
};

#endif /* EXTERNAL_MODEL_H_ */




