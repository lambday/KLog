/// -*- c++ -*-
/// @file   LibSVM_Model.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Wrapper around libsvm solvers

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

#ifndef LIBSVM_MODEL_H
#define LIBSVM_MODEL_H

#include "Model.h"
#include "Performance.h"
#include "FlagsService.h"
#include "libsvm/svm.h"

#include "Performance.h"

class LibSVM_DatasetWrapper
{
private:
  svm_problem prob;
  std::vector<std::string> case_ids;

  void count_cases(const std::vector<std::string>& interpretation_ids) {
    check_sorted_ascending(interpretation_ids); // or the trick below does not work
    std::vector<std::string>::const_iterator ii = interpretation_ids.begin();
    Cases_T::const_iterator di = The_Dataset.get_cases().begin();
    prob.l = 0;
    while (ii != interpretation_ids.end() && di != The_Dataset.get_cases().end()) {
      if (*ii < di->first) { 
        ++ii;
      } else if (di->first < *ii) {
        ++di;
      } else {
        // std::set<std::string> c = di->second; // the set of cases in this interpretation
        //***
        std::vector<std::string> c = di->second; // the set of cases in this interpretation
        //***
        // for (std::set<std::string>::const_iterator i=c.begin(); i!=c.end(); ++i) {
        //***
        for (std::vector<std::string>::const_iterator i=c.begin(); i!=c.end(); ++i) {
        //***
          // ***
          if (The_Dataset.get_rejected(*i))
            continue;
          // ***
          prob.l++;
        }
        ++di; ++ii;
      }
    }
  }

public:
  /// Creates a data set wrapper for libsvm, given a set of
  /// interpretation ids. The wrapper incapsulates the svm problem
  /// internal data structure used by libsvm
  ///
  /// @param interpretation_ids 
  ///
  LibSVM_DatasetWrapper(const std::vector<std::string>& interpretation_ids) {
    count_cases(interpretation_ids);
    std::vector<std::string>::const_iterator ii = interpretation_ids.begin();
    Cases_T::const_iterator di = The_Dataset.get_cases().begin();
    prob.x = Malloc(svm_node*,prob.l);
    prob.y = Malloc(double,prob.l);
    int index=0;
    case_ids.clear();
    while (ii != interpretation_ids.end() && di != The_Dataset.get_cases().end()) {
      if (*ii < di->first) { 
        ++ii;
      } else if (di->first < *ii) {
        ++di;
      } else {
        // std::set<std::string> c = di->second; // the set of cases in this interpretation
        //***
        std::vector<std::string> c = di->second; // the set of cases in this interpretation
        //***
        // for (std::set<std::string>::const_iterator i=c.begin(); i!=c.end(); ++i) {
        //***
        for (std::vector<std::string>::const_iterator i=c.begin(); i!=c.end(); ++i) {
        //***
          LibSVM_CaseWrapper* cw;
          if ((cw=dynamic_cast<LibSVM_CaseWrapper*>(The_Dataset.get_sparse_vector(*i))) == NULL) {
            throw std::runtime_error("Nasty bug - dynamic_cast failed in LibSVM_DatasetWrapper() constructor");
          }
          // ***
          if (The_Dataset.get_rejected(*i))
            continue;
          // ***
          prob.x[index] = cw->get_vector();
          prob.y[index] = The_Dataset.get_label(*i);
          case_ids.push_back(*i);
          index++;
        }
        ++di; ++ii;
      }
    }
  }
  svm_problem& get_prob() {return prob;}
  std::vector<std::string>& get_case_ids() {return case_ids;}
  ~LibSVM_DatasetWrapper() {
    free(prob.x);
    free(prob.y);
  }
};


/// 
///
/// @param model The svm model
/// @param x the feature vector
/// @param margin for one-class and regression, f(x); for binary
/// classification y*f(x); for multiclass the predicted class index
/// (actually an integer)
///
/// @return same as svm_predict_values, i.e. f(x) for one class and
/// regression, and integer label for classification
///
double svm_predict_with_margin(const svm_model *model, const svm_node *x, double& margin)
{
  int nr_class = model->nr_class;
  double *dec_values;
  if(model->param.svm_type == ONE_CLASS ||
     model->param.svm_type == EPSILON_SVR ||
     model->param.svm_type == NU_SVR)
    dec_values = Malloc(double, 1);
  else 
    dec_values = Malloc(double, nr_class*(nr_class-1)/2);
  double pred_result = svm_predict_values(model, x, dec_values);
  if(model->param.svm_type == ONE_CLASS ||
     model->param.svm_type == EPSILON_SVR ||
     model->param.svm_type == NU_SVR) {
    margin = dec_values[0];
    free(dec_values);
    return margin;
  }
  if (nr_class==2) {
    margin = dec_values[0] * model->label[0];
  } else {
    // margin = svm_predict(model,x);
    margin = pred_result; // Fri Apr 22 16:00:54 2011 not clear why I was calling predict twice here..
    // KLOG_THROW("nr_class==" << nr_class << " - Multiclass is presently unsupported");
  }
  free(dec_values);
  return pred_result;
}



template<typename PerformanceReporter>
class Libsvm_Model: public Model, public FlagsServiceClient
{
protected:
  struct svm_parameter param; // Used internally by libsvm, attached to flags
  // struct svm_problem* prob; // Used internally by libsvm
  struct svm_model *model; // Filled in by libsvm train
  // Use kLog reporters
  /** Global performance reporter (for k-fold CV etc). */
  static PerformanceReporter global_reporter;
  /** Local performance reporter (for individual folds). */
  PerformanceReporter reporter;

public:
  Libsvm_Model(const std::string& id, int type) : Model(id),FlagsServiceClient(id),reporter("Local report")
  {
    model_type = "libsvm";
    new_flag(&param.kernel_type,"kernel_type",
             "(unsigned)\n0: u'*v, 1: (gamma*u'*v + coef0)^degree, 2: exp(-gamma*|u-v|^2), 3: tanh(gamma*u'*v + coef0)");
    new_flag(&param.degree, "degree","(int)\nFor polynomial kernel");
    new_flag(&param.gamma, "gamma","(double)\nFor poly/rbf/sigmoid kernels");
    new_flag(&param.coef0, "coef0","(double)\nFor poly/sigmoid kernels");
    new_flag(&param.cache_size, "cache_size","(double)\nCache memory size in MB");
    new_flag(&param.C, "c","(double)\nRegularization parameter");
    new_flag(&param.nu, "nu","(double)\nFor nu-SVC, one-class SVM, and nu-SVR");
    new_flag(&param.p, "p","(double)\nepsilon in loss function of epsilon-SVR");
    new_flag(&param.probability, "probability","(int)\nEstimate conditional probabilities");

    // Default values
    param.svm_type = type;
    param.kernel_type = 0;
    param.degree = 3;
    param.gamma = 1;
    param.coef0 = 0;
    param.nu = 0.5;
    param.cache_size = 100;
    param.C = 1;
    param.eps = 1e-3;
    param.p = 0.1;
    param.shrinking = 1;
    param.probability = 0;
    param.nr_weight = 0;
    param.weight_label = NULL;
    param.weight = NULL;

    param.nr_weight = 9;
    param.weight_label = (int *)realloc(param.weight_label,sizeof(int)*param.nr_weight);
    param.weight = (double *)realloc(param.weight,sizeof(double)*param.nr_weight);
    param.weight_label[0] = -1;
    param.weight[0] = 1.0; // needs to be set explicitly for binary classification
    for (int i = 1; i<param.nr_weight; ++i) {
      param.weight_label[i] = i;
      param.weight[i] = 1.0;
    }
    new_flag(&(param.weight[0]), "wn","(double)\nC weight for - class in binary classification");
    new_flag(&(param.weight[1]), "wp","(double)\nC weight for + class in binary classification");
    new_flag(&(param.weight[1]), "w1","(double)\nC weight for class 1 in multiclass classification");
    new_flag(&(param.weight[2]), "w2","(double)\nC weight for class 2 in multiclass classification");
    new_flag(&(param.weight[3]), "w3","(double)\nC weight for class 3 in multiclass classification");
    new_flag(&(param.weight[4]), "w4","(double)\nC weight for class 4 in multiclass classification");
    new_flag(&(param.weight[5]), "w5","(double)\nC weight for class 5 in multiclass classification");
    new_flag(&(param.weight[6]), "w6","(double)\nC weight for class 6 in multiclass classification");
    new_flag(&(param.weight[7]), "w7","(double)\nC weight for class 7 in multiclass classification");
    new_flag(&(param.weight[8]), "w8","(double)\nC weight for class 8 in multiclass classification");
       
    model = NULL;
    reporter.reset();
  }

  virtual void train(const std::vector<std::string>& interpretation_ids){
    LibSVM_DatasetWrapper dataset_wrapper(interpretation_ids);
    const char* error_msg = svm_check_parameter(&(dataset_wrapper.get_prob()),&param);
    if (error_msg)
      KLOG_THROW("Invalid parameters for libsvm train: >" << error_msg << "<");
    model = svm_train(&(dataset_wrapper.get_prob()),&param);
  };
  virtual void test_dataset(const std::vector<std::string>& interpretation_ids,bool newdata){
    if (newdata)
      reporter.reset("Local reporter  (on new data)");
    else
      reporter.reset("Local reporter  (on train data)");
    LibSVM_DatasetWrapper wrapper(interpretation_ids);
    for (int i=0; i<wrapper.get_prob().l; i++) {
      double f;
      svm_predict_with_margin(model,wrapper.get_prob().x[i],f);
      reporter.add_case(f,wrapper.get_prob().y[i],wrapper.get_case_ids().at(i));
      if (newdata)
        global_reporter.add_case(f,wrapper.get_prob().y[i],wrapper.get_case_ids().at(i));
    }
    // reporter.report(std::cout);
  };

  virtual void reset_reporter(const std::string& r, const std::string& desc=""){
    if (r=="local")
      reporter.reset(desc);
    else if (r=="global")
      global_reporter.reset(desc);
    else
      KLOG_THROW("Invalid reporter ID >" << r << "<");
  };
  virtual void report(std::ostream& os,const std::string& r){
    if (r=="local")
      reporter.report(os);
    else if (r=="global")
      global_reporter.report(os);
    else
      KLOG_THROW("Invalid reporter ID >" << r << "<");
  };
  virtual void save_predictions(const std::string& dir,const std::string& r){
    if (r=="local")
      reporter.save_predictions(dir);
    else if (r=="global")
      global_reporter.save_predictions(dir);
    else
      KLOG_THROW("Invalid reporter ID >" << r << "<");
  };
  virtual void load(const std::string filename){
    KLOG_THROW("Not implemented yet...");
  }
  virtual void save(const std::string filename){
    if (model==NULL)
      KLOG_THROW("Can't save untrained model");
    if (svm_save_model(filename.c_str(),model))
      KLOG_THROW("Cannot save model in >" << filename << "<");
  }
  virtual bool check_ability(const std::string ability){
    if (ability=="binary_classification")
      return (param.svm_type==0 || param.svm_type==1) && (dynamic_cast<BinaryClassifierReporter*>(&reporter) != NULL);
    else if (ability=="multiclass_classification")
      return (param.svm_type==0 || param.svm_type==1)  && (dynamic_cast<MulticlassClassifierReporter*>(&reporter) != NULL);
    else if (ability=="regression")
      return (param.svm_type==3 || param.svm_type==4)  && (dynamic_cast<RegressionReporter*>(&reporter) != NULL);
    else
      return false;
  };
  
  virtual ~Libsvm_Model() {
    svm_free_and_destroy_model(&model);
  }

  double get_prediction(const std::string& caseid) {
    return global_reporter.get_prediction(caseid);
  }
};


#endif // LIBSVM_MODEL_H
