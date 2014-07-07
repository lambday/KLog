/// -*- c++ -*-
/// @file   Performance.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Set of classes used to measure case-level prediction performance.  

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


// A performance reporter is simply a class with three
// public methods called reset, add_case, and report. No hierarchy
// (there is little to share), just add new reporter classes with the
// same public interface.  Given prediction f -- i.e. f(x) -- and
// target y, add_case simply "remembers" the pair for subsequent
// performance evaluation. Use reset() to forget previously seen
// cases. Method report() prints on the given stream a number of
// performance measures, depending on the task.  These classes are
// meant to be used as template parameters for instantiating model
// classes (e.g. LB_ScalarModel and LibSVM_Model have a template
// parameter for the performance reporter). Predefined models
// (complete with a performance reporter) can be added in ModelPool
// and assigned an identifier string (a Prolog atom) used in kLog
// new_model/2 predicate.

#ifndef PERFORMANCE_H_
#define PERFORMANCE_H_

#include <iostream>
#include <fstream>
#include <iomanip>
#include <limits>
#include <vector>
#include <tr1/unordered_map>

#include "kLogMaster.h"

class Reporter {
protected:
    typedef std::tr1::unordered_map<std::string, double> String2DoubleMap;
    std::string description;
    String2DoubleMap predictions;
    String2DoubleMap labels;
private:

    virtual void dummy(void) {
    }; // make dynamic_cast possible...
public:

    double get_prediction(const std::string& caseid) {
        String2DoubleMap::const_iterator found = predictions.find(caseid);
        if (found == predictions.end()) {
            KLOG_THROW("Case id >" << caseid << "< has no associated prediction (yet)");
        }
        return found->second;
    }
};

/**
   Binary classification. Do contingency table, accuracy, precision,
   recall, F1, AUROC/AURPC analysis. AUC depends on AUCCalculator by
   Jesse Davis and Mark Goadrich
   (http://mark.goadrich.com/programs/AUC/) which must be present in
   the kLog installation path.
 */
class BinaryClassifierReporter : public Reporter {
private:
    unsigned nerr;
    unsigned tp;
    unsigned fp;
    unsigned tn;
    unsigned fn;
    unsigned n_cases;

public:

    BinaryClassifierReporter(const std::string& _description = "") {
        reset(_description);
    }

    void reset(const std::string& _description = "") {
        description = _description;
        nerr = tp = fp = tn = fn = n_cases = 0;
        predictions.clear();
        labels.clear();
    }

    void add_case(double f, double y, const std::string& caseid) {
        predictions[caseid] = f;
        labels[caseid] = y;
        if (f > 0) {
            if (y > 0) ++tp;
            else ++fp;
        } else {
            if (y > 0) ++fn;
            else ++tn;
        }
        ++n_cases;
        nerr = fp + fn;
    }

    void report(std::ostream& os) {
        //double accuracy = double(tp+tn)/double(n);
        double precision = double(tp) / double(tp + fp);
        double recall = double(tp) / double(tp + fn);
        double f1 = 2 * precision * recall / (precision + recall);
        os << "===== performance report (" << description << ") =====" << std::endl;
        os.setf(ios::fixed);
        os << "# Cases: "
                << std::setw(5)
                << n_cases << std::endl;
        os << "Error:      "
                << std::setw(5) << std::setprecision(2)
                << (double) nerr * 100.0 / n_cases << "%" << std::endl;
        os << "Precision:  "
                << std::setw(5) << std::setprecision(2)
                << precision * 100.0 << "%" << std::endl;
        os << "Recall:     "
                << std::setw(5) << std::setprecision(2)
                << recall * 100.0 << "%" << std::endl;
        os << "F1-measure: "
                << std::setw(5) << std::setprecision(2)
                << f1 * 100.0 << "%" << std::endl;
        os << "Contingency table:" << std::endl;
        os.width(0);
        os << "           ";
        os.width(8);
        os << std::right << '+';
        os.width(8);
        os << std::right << '-' << std::endl;
        os.width(0);
        os << "Predicted +";
        os.width(8);
        os << std::right << tp;
        os.width(8);
        os << std::right << fp << std::endl;
        os.width(0);
        os << "          -";
        os.width(8);
        os << std::right << fn;
        os.width(8);
        os << std::right << tn << std::endl;
        os << std::endl;
        os.width(0);
        os << "========= end report (" << description << ") =========" << std::endl;
    }

    void save_predictions(const std::string& dir) {
        std::string logfile = dir + "/output.log";
        std::string pllogfile = dir + "/output.pl";
        std::string yyyfile = dir + "/output.yyy"; // for AUC analysis
        std::ofstream log_os(logfile.c_str());
        std::ofstream pllog_os(pllogfile.c_str());
        std::ofstream yyy_os(yyyfile.c_str());
        for (String2DoubleMap::const_iterator i = labels.begin(); i != labels.end(); ++i) {
            log_os << predictions[i->first] << " " << i->second
                    << " # " << i->first << std::endl;
            pllog_os << "outputlog(" << predictions[i->first] << "," << i->second
                    << "," << i->first << ")." << std::endl;
            yyy_os << predictions[i->first] << " "
                    << (i->second > 0 ? 1 : 0) << std::endl;
        }
        log_os.close();
        pllog_os.close();
        yyy_os.close();
        // Also do AUC analysis
        std::ostringstream outs;
        outs << "java -jar " << The_Master.get_install_directory() << "/../bin/auc.jar "
                << dir << "/output.yyy LIST > " << dir << "/auc.log";
        system(outs.str().c_str());
    }
};

/**
 * External binary classifier. Get predictions from .out file assuming
 * it contains a margin for each case.
 * 
 */
class ExternalBinaryClassifierReporter : public BinaryClassifierReporter {
public:

    ExternalBinaryClassifierReporter(const std::string& _description = "") : BinaryClassifierReporter(_description) {
    }

    void read_predictions(const std::string& basename) {
        std::ifstream out_stream;
        out_stream.open((basename + ".out").c_str(), ios::out);
        std::ifstream cid_stream;
        cid_stream.open((basename + ".cid").c_str(), ios::out);
        while (out_stream.good()) {
            double f, y;
            std::string caseid, dummy;
            out_stream >> f;
            cid_stream >> caseid >> dummy >> y;
            add_case(f, y, caseid);
        }
    }
};

/**
   Multiclass classification. Do contingency table, accuracy.
 */

class MulticlassClassifierReporter : public Reporter {
private:
    typedef std::map<int, int> IntInt_T;
    typedef std::map<int, IntInt_T> CTable_T;
    CTable_T contingency_table;
    unsigned nerr;
    unsigned n_cases;
    bool initialized;

    int last_class_index() {
        int last = 0;
        for (NumLabelMap_T::const_iterator i = The_Dataset.get_num_label().begin(); i != The_Dataset.get_num_label().end(); ++i) {
            // std::cout << i->first << "->" << i->second << std::endl;
            if (i->second > last) last = i->second;
        }
        return last;
    }

    void init_ctable() {
        int last = last_class_index();
        for (int f = 1; f <= last; ++f)
            for (int y = 1; y <= last; ++y)
                contingency_table[f][y] = 0;
        initialized = true;
    }
public:

    MulticlassClassifierReporter(const std::string& _description = "") {
        reset(_description);
        // int nc = 0;
        // for (NumLabelMap_T::const_iterator i=The_Dataset.get_num_label().begin(); i!=The_Dataset.get_num_label().end(); ++i) {
        //   std::cout << i->first << "->" << i->second << std::endl;
        //   if (i->second>nc) nc=i->second;
        // }
        // for (SymLabelMap_T::const_iterator i=The_Dataset.get_sym_label().begin(); i!=The_Dataset.get_sym_label().end(); ++i) {
        //   std::cout << i->first << "->" << i->second << std::endl;
        // }
        // for (unsigned i=0; i<nc+1; ++i) {
        //   std::vector<int> ti(nc+1);
        //   contingency_table.push_back(ti);
        // }
    }

    void reset(const std::string& _description = "") {
        description = _description;
        for (CTable_T::iterator i = contingency_table.begin(); i != contingency_table.end(); ++i)
            i->second.clear();
        contingency_table.clear();
        nerr = n_cases = 0;
        predictions.clear();
        labels.clear();
        initialized = false;
    }

    void add_case(double f, double y, const std::string& caseid) {
        if (!initialized) init_ctable();
        predictions[caseid] = f;
        labels[caseid] = y;
        contingency_table[int(f)][int(y)]++;
        // printf("c[%d][%d]=%d\n",int(f),int(y),contingency_table[int(f)][int(y)]);
        n_cases++;
        nerr += int(f) != int(y);
    }

    void report(std::ostream& os) {
        os << "===== performance report (" << description << ") =====" << std::endl;
        os.setf(ios::fixed);
        os << "# Cases: "
                << std::setw(5)
                << n_cases << std::endl;
        os << "Case error rate:           "
                << std::setw(5) << std::setprecision(2)
                << (double) nerr * 100.0 / n_cases << "%" << std::endl;

        String2DoubleMap errors_per_interpretation;
        for (String2DoubleMap::const_iterator i = labels.begin(); i != labels.end(); ++i) {
            size_t l = (i->first).rfind('/');
            std::string intid = (i->first).substr(0, l);
            errors_per_interpretation[intid] += predictions[i->first] != i->second;
        }
        int n_wrong = 0;
        for (String2DoubleMap::const_iterator i = errors_per_interpretation.begin();
                i != errors_per_interpretation.end(); ++i) {
            if (i->second > 0)
                n_wrong++;
            // std::cout << i->first << " " << i->second << std::endl;
        }
        os << "Interpretation error rate: "
                << std::setw(5) << std::setprecision(2)
                << (double) n_wrong * 100.0 / errors_per_interpretation.size()
                << "%" << std::endl;

        os << "Contingency table: (rows are predictions)" << std::endl;
        SymLabelMap_T sym_label = The_Dataset.get_sym_label();
        // for (SymLabelMap_T::iterator ii=sym_label.begin(); ii!=sym_label.end(); ++ii)
        //   std::cout << "Symlabel " << ii->first << " " << ii->second << std::endl;
        // int last=last_class_index();
        os.width(0);
        os << "           ";
        for (CTable_T::iterator f = contingency_table.begin(); f != contingency_table.end(); ++f) {
            os.width(8);
            os << std::right << sym_label[f->first].substr(0, 7);
        }
        os << std::endl;

        unsigned n_cases_per_class[1014] = {0};
        unsigned n_predictions_per_class[1024] = {0};
        unsigned tp_per_class[1024] = {0};
        unsigned n_classes = 0;
        for (CTable_T::iterator f = contingency_table.begin(); f != contingency_table.end(); ++f) {
            n_classes++;
            for (IntInt_T::iterator y = f->second.begin(); y != f->second.end(); ++y) {
                n_cases_per_class[y->first] += y->second;
                n_predictions_per_class[f->first] += y->second;
                if (y->first == f->first)
                    tp_per_class[y->first] += y->second;
            }
        }
        double average_precision = 0.0;
        double average_recall = 0.0;
        double average_f1 = 0.0;
        double average_multiclass_precision = 0.0;
        double average_multiclass_recall = 0.0;
        double average_multiclass_f1 = 0.0;
        for (CTable_T::iterator f = contingency_table.begin(); f != contingency_table.end(); ++f) {
            os.width(8);
            os << std::right << sym_label[f->first].substr(0, 7);
            for (IntInt_T::iterator y = f->second.begin(); y != f->second.end(); ++y) {
                os.width(8);
                os << std::right << y->second;
            }
            double tn = double(n_cases - nerr - tp_per_class[f->first]);
            double fp = double(n_predictions_per_class[f->first] - tp_per_class[f->first]);
            double fn = double(n_cases_per_class[f->first] - tp_per_class[f->first]);
            double acc = (tp_per_class[f->first] + tn) / (tp_per_class[f->first] + tn + fp + fn);
            double pr = double(tp_per_class[f->first]) / double(n_predictions_per_class[f->first]);
            double rc = double(tp_per_class[f->first]) / double(n_cases_per_class[f->first]);
            os << "    a,p,r,f1="
                    // << "[" << << n_cases_per_class[f->first] << "," << n_predictions_per_class[f->first] << "]"
                    << " " << std::setprecision(3) << acc
                    << " " << std::setprecision(3) << pr
                    << " " << std::setprecision(3) << rc
                    << " " << std::setprecision(3) << 2 * pr * rc / (pr + rc);
            os << std::endl;
            average_precision += pr * n_cases_per_class[f->first] / n_cases;
            average_recall += rc * n_cases_per_class[f->first] / n_cases;
            average_f1 += 2 * pr * rc / (pr + rc) * n_cases_per_class[f->first] / n_cases;
            average_multiclass_precision += pr;
            average_multiclass_recall += rc;
            average_multiclass_f1 += 2 * pr * rc / (pr + rc);
        }
        average_multiclass_precision /= double(n_classes);
        average_multiclass_recall /= double(n_classes);
        average_multiclass_f1 /= double(n_classes);

        os << "Average  (weighted)  p,r,f1 ="
                << " " << std::setprecision(3) << average_precision
                << " " << std::setprecision(3) << average_recall
                << " " << std::setprecision(3) << average_f1
                << std::endl;
        os << "Average (multiclass) p,r,f1 ="
                << " " << std::setprecision(3) << average_multiclass_precision
                << " " << std::setprecision(3) << average_multiclass_recall
                << " " << std::setprecision(3) << average_multiclass_f1
                << std::endl;
        os.width(0);
        os << "========= end report (" << description << ") =========" << std::endl;
    }

    void save_predictions(const std::string& dir) {
        SymLabelMap_T sym_label = The_Dataset.get_sym_label();
        std::string logfile = dir + "/output.log";
        std::ofstream log_os(logfile.c_str());
        for (String2DoubleMap::const_iterator i = labels.begin(); i != labels.end(); ++i) {
            log_os << predictions[i->first] << " " << sym_label[predictions[i->first]] << " "
                    << i->second << " " << sym_label[i->second]
                    << " # " << i->first << std::endl;
        }
        log_os.close();
    }

};

/**
 * External multiclass classifier. Get predictions from .out file
 * assuming it contains an integer class label for each case.
 * 
 */
class ExternalMulticlassClassifierReporter : public MulticlassClassifierReporter {
public:

    ExternalMulticlassClassifierReporter(const std::string& _description = "") : MulticlassClassifierReporter(_description) {
    }

    void read_predictions(const std::string& basename) {
        std::ifstream out_stream;
        out_stream.open((basename + ".out").c_str(), ios::out);
        std::ifstream cid_stream;
        cid_stream.open((basename + ".cid").c_str(), ios::out);
        while (out_stream.good()) {
            double f, y;
            std::string caseid, dummy;
            out_stream >> f;
            cid_stream >> caseid >> dummy >> y;
            add_case(f, y, caseid);
        }
    }
};

/**
   Simple single task regression. Do MSE, RMSE, MAE, MAPE, correlation
   coefficient.
 */
class RegressionReporter : public Reporter {
private:
    double mse; // Mean squared error
    double mae; // Mean absolute error
    double mape; // mean absolute percentage error
    double mape_threshold; // Don't include cases if target is below threshold
    double sump, sumt, sumpp, sumtt, sumpt; // for correlation coefficient
    unsigned n_cases;
    unsigned n_cases_mape;
public:

    RegressionReporter(const std::string& _description = "") {
        mape_threshold = 1e-10;
        reset(_description);
    }

    void reset(const std::string& _description = "") {
        description = _description;
        mse = mae = mape = 0.0;
        n_cases = n_cases_mape = 0;
        sump = sumt = sumpp = sumtt = sumpt = 0.0;
        predictions.clear();
        labels.clear();
    }

    void add_case(double f, double y, const std::string& caseid) {
        predictions[caseid] = f;
        labels[caseid] = y;
        mse += (f - y)*(f - y);
        sump += f;
        sumt += y;
        sumpp += f*f;
        sumtt += y*y;
        sumpt += f*y;
        mae += fabs(f - y);
        if (y > mape_threshold) {
            mape += fabs((f - y) / y);
            n_cases_mape++;
        }
        n_cases++;
    }

    void report(std::ostream& os) {
        os << "===== performance report (" << description << ") =====" << std::endl;
        os << "Tested " << n_cases << " cases." << std::endl;
        os << std::setprecision(4)
                << "Mean squared error: " << mse / double(n_cases) << std::endl;
        os << std::setprecision(4)
                << "Squared correlation coefficient: " <<
                ((n_cases * sumpt - sump * sumt)*(n_cases * sumpt - sump * sumt)) /
                ((n_cases * sumpp - sump * sump)*(n_cases * sumtt - sumt * sumt))
                << std::endl;
        os << std::setprecision(4)
                << "RMSE: " << sqrt(mse / double(n_cases)) << std::endl;
        os << std::setprecision(4)
                << "MAE: " << mae / double(n_cases) << std::endl;
        os << std::setprecision(4)
                << "MAPE: " << 100 * mape / double(n_cases_mape)
                << "% (" << n_cases_mape << ")" << std::endl;
        os << "========= end report (" << description << ") =========" << std::endl;
    }

    void save_predictions(const std::string& dir) {
        std::string logfile = dir + "/output.log";
        std::ofstream log_os(logfile.c_str());
        for (String2DoubleMap::const_iterator i = labels.begin(); i != labels.end(); ++i) {
            log_os << predictions[i->first] << " " << i->second
                    << " # " << i->first << std::endl;
        }
        log_os.close();
    }
};

#endif // PERFORMANCE_H_
