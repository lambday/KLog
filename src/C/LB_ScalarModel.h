/// -*- c++ -*-
/// @file   LB_ScalarModel.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Stochastic gradient descent for scalar predictions
///         (binary classification or regression) with L2 regularization.
///         Wraps around code by Leon Bottou

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


#ifndef LB_SCALARMODEL_H_
#define LB_SCALARMODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "vectors.h"
#include "timer.h"
#include "Model.h"
#include "Loss.h"
#include "FlagsService.h"
#include "Performance.h"
#include "Utility.h"
#include "Dataset.h"

typedef std::vector<SVector*> xvec_t;
typedef std::vector<double> yvec_t;


class LB_ScalarModel_DatasetWrapper
{
public:
  xvec_t slice_x;
  yvec_t slice_y;
  std::vector<std::string> case_ids;
  /**
   * given a set of interpretation ids, retrieve the corresponding set
   * of sparse vectors by searching all the cases associated with each
   * interpretation in the sparse vector database. The results are
   * directly usable for SVM or other propositional learner.
   * @param interpretation_ids <vector of strings>
   * @param slice_x <retrieved vector of pointers to sparse vectors>
   * @param slice_y <retrieved vector of target labels>
   * @param slice_ids <retrieved vector of case ids>
   *
   * Remark: ``slice'' in the present context has not the same meaning
   * as in sliced interpretations.
   */
  LB_ScalarModel_DatasetWrapper(const std::vector<std::string>& interpretation_ids)
  {
    check_sorted_ascending(interpretation_ids); // or the trick below does not work
    std::vector<std::string>::const_iterator ii = interpretation_ids.begin();
    Cases_T::const_iterator di = The_Dataset.get_cases().begin();
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
          LB_CaseWrapper* c;
          if ((c=dynamic_cast<LB_CaseWrapper*>(The_Dataset.get_sparse_vector(*i))) == NULL) {
            KLOG_THROW("Nasty bug - dynamic_cast failed with caseID >" << *i << "<");
          }
          // ***
          if (The_Dataset.get_rejected(*i))
            continue;
          // ***
          slice_x.push_back(c->get_vector());
          slice_y.push_back(The_Dataset.get_label(*i));
          case_ids.push_back(*i);
        }
        ++di; ++ii;
      }
    }
  }
};



// ============== Stochastic Gradient Descent scalar models =============


// Zero when no bias, one when bias term
#define BIAS 1

/** 
 * Linear model for scalar predictions trained by stochastic gradient
 * descent, using L2 regularization. This class heavily relies on Léon
 * Bottou's code for SVM (http://leon.bottou.org/projects/sgd). The
 * template parameter is a loss function class: it should contain two
 * functionals called loss and dloss. The associated functions should
 * take two doubles (prediction and target, respectively) and return a
 * double (either the loss or its derivative). HingeLoss (SVM),
 * SoftHingeLoss (logistic regression) and SquaredLoss (regression)
 * are examples of valid loss classes defined in Loss.h The template
 * parameter PerformanceReporter is used to produce performance
 * reports, e.g. a contingency table with precision, recall, F1 for
 * binary classification, AUC analysis, or RMSE, MAD, MAPE etc for
 * regression; valid classes are defined in Performance.h and should
 * offer a common interface.
 */

template<typename LossFunction, typename PerformanceReporter>
class LB_ScalarModel : public Model, public FlagsServiceClient
{
 private:
  /** Learning rate adjustment factor */
  double  t;
  /** Regularization coefficient (default = 0.01) */
  double  lambda;
  /** Ratio of -ive vs +ive loss (default = 1.0) */
  double  lossratio;
  /** Hyperplane */
  SVector w;
  /** Scaling factor for w */
  double  wscale;
  /** Offset of the hyperplane. */
  double  bias;
  /** Number of training epochs (default = 5). */
  int     epochs;

  /** Global performance reporter. The reason for this reporter is
      that often we do kfold CV and besides measuring performance in
      each fold, we want the (microaveraged) overall performance so we
      have to remember all predictions on the entire data
      set. Basically this subsumes previously used Kurt's code
      (eval_performance/3 in learn.pl) that only worked for binary
      classification.
   */
  static PerformanceReporter global_reporter;
  /** Local performance reporter (for individual folds). */
  PerformanceReporter reporter;

  /** function objects representing the loss function and its derivative */
  typename LossFunction::loss myloss;
  typename LossFunction::dloss mydloss;
 public:
 /**
  * Creates a new LB_ScalarModel model. All parameters are given default values:
  * 
  * @param id <model identifier>
  */
  LB_ScalarModel(const std::string& id) : Model(id),FlagsServiceClient(id),reporter("Local report") {
    model_type = "lb";
    new_flag(&epochs, "epochs","(unsigned)\nNumber of epochs for SGD");
    new_flag(&lambda, "lambda","(double)\nRegularization parameter for SGD");
    new_flag(&lossratio, "lossratio","(double)\nRatio of -ive vs +ive loss for SGD");
    epochs = 5;
    lambda = 0.001;
    lossratio = 1.0;
    reporter.reset();
  }

  /**
   * Train the model
   * @param interpretation_ids <vector of interpretation identifiers>
   */
  void train(const std::vector<std::string>& interpretation_ids) {
    LB_ScalarModel_DatasetWrapper dataset_wrapper(interpretation_ids);
    wscale = 1.0;
    bias = 0.0;
    w.clear();
    // Shift t in order to have a reasonable initial learning
    // rate. This assumes |x| \approx 1.
    double maxw = 1.0 / sqrt(lambda);
    double typw = sqrt(maxw);
    double eta0 = typw / max(1.0, mydloss(1,-typw));
    t = 1 / (eta0 * lambda);
    std::cout << "maxw= " << maxw 
              << " typw= " << typw
              << " eta0= " << eta0
              << " t= " << t << std::endl;
    if (dataset_wrapper.slice_x.size()!=dataset_wrapper.slice_y.size())
      KLOG_THROW("x and y sizes differ >" << dataset_wrapper.slice_x.size() << "!=" << dataset_wrapper.slice_y.size()<<"<");
    train(0,dataset_wrapper.slice_x.size()-1,dataset_wrapper.slice_x,dataset_wrapper.slice_y,dataset_wrapper.case_ids);
  }
  /**
   * Classify a data set
   * @param interpretation_ids <vector of interpretation identifiers>
   */
  void test_dataset(const std::vector<std::string>& interpretation_ids, bool newdata) {
    LB_ScalarModel_DatasetWrapper wrapper(interpretation_ids);
    if (wrapper.slice_x.size()!=wrapper.slice_y.size())
      KLOG_THROW("x and y sizes differ >" << wrapper.slice_x.size() << "!=" << wrapper.slice_y.size()<<"<");
    test_dataset(0,wrapper.slice_x.size()-1,wrapper.slice_x,wrapper.slice_y,wrapper.case_ids, newdata);
  }

  /** 
   * Reset a performance reporter
   * @param r <reporter identifier>
   */
  void reset_reporter(const std::string& r, const std::string& desc="") {
    if (r=="local")
      reporter.reset(desc);
    else if (r=="global")
      global_reporter.reset(desc);
    else
      KLOG_THROW("Invalid reporter ID >" << r << "<");
  }
  /** 
   * Get a report on individually tested cases using a performance
   * reporter
   */
  void report(std::ostream& os,const std::string& r) {
    if (r=="local")
      reporter.report(os);
    else if (r=="global")
      global_reporter.report(os);
    else
      KLOG_THROW("Invalid reporter ID >" << r << "<");
  }
  /** 
   * Save predictions stored in the global performance reporter
   */
  void save_predictions(const std::string& dir,const std::string& r) {
    if (r=="local")
      reporter.save_predictions(dir);
    else if (r=="global")
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
    oss << "wscale: " << wscale << std::endl;
    oss << "bias: " << bias << std::endl;
    oss << "w: " << w << std::endl;
    return oss.str();
  }
  /**
   * Load a model from a file. The expected format is the same produced by save()
   * @param filename <filename>
   */
  void load(std::string filename) {
    std::ifstream is;
    is.open(filename.c_str(), std::ifstream::in);
    while (is.good()) {
      std::string key;
      is >> key;
      if (key=="") continue;
      if (key=="Model:")
        is >> model_id;
      else if (key=="wscale:")
        is >> wscale;
      else if (key=="bias:")
        is >> bias;
      else if (key=="w:")
        is >> w;
      else {
        KLOG_THROW("Invalid key >" << key << "< in model file <" << filename << "<");
      }
    }
  }
  /**
   * Save a model to file. The saved model can be read by load()
   * @param filename <doc>
   */
  void save(std::string filename) {
    std::ofstream os(filename.c_str());
    os << str();
    os.close();
  }
  bool check_ability(std::string ab) {
    return LossFunction::ability(ab);
  }
private:
  void train(int imin, int imax, const xvec_t& xp, const yvec_t &yp, const std::vector<std::string>& ids) {
    Timer timer;
    timer.start();
    std::vector<int> perm(imax-imin+1);
    for (int i=imin; i<=imax; i++)
      perm[i-imin] = i-imin;
    std::random_shuffle(perm.begin(), perm.end());
    for(int i=0; i<epochs; i++) {
      std::cout << "--------- Epoch " << i+1 << "." << std::endl;
      do_epoch(imin, imax, xp, yp, perm);
      std::cout << "Total training time " << std::setprecision(6) 
                << timer.elapsed() << " secs." << std::endl;
      test_dataset(imin, imax, xp, yp, ids, false);
      // should we test() here or from outside??
    }
    timer.stop();
  }
  void test_dataset(int imin, int imax, const xvec_t& xp, const yvec_t &yp, const std::vector<std::string>& ids, bool newdata) {
    double cost = 0;
    if (newdata)
      reporter.reset("Local reporter  (on new data)");
    else
      reporter.reset("Local reporter  (on train data)");
    for (int i=imin; i<=imax; i++) {
      const SVector &x = *(xp.at(i));
      //std::cout << "x[" << i << "]= " << x << std::endl;
      double y = yp.at(i);
      double wx = dot(w,x) * wscale;
      double f = (wx + bias);
      cost +=  myloss(f,y) * (y<0? lossratio:1.0);
      // std::cout << "Test case " << i
      //           << " y= " << y
      //           << " f= " << f
      //           << std::endl;
      reporter.add_case(f,y,ids.at(i));
      if (newdata)
        global_reporter.add_case(f,y,ids.at(i));
    }
    int n = imax - imin + 1;
    double wnorm =  dot(w,w) * wscale * wscale;
    cost = cost / n + 0.5 * lambda * wnorm;
    std::cout << "Cost (regularized average loss): " << cost << std::endl;
    // reporter.report(std::cout);
  }
  /**
   * Perform a single epoch of SGD training
   * @param imin <index of first data point>
   * @param imax <index of last data point>
   * @param xp <input vectors>
   * @param yp <target labels>
   */
  void batch_do_epoch(int imin, int imax, const xvec_t& xp, const yvec_t &yp) {
    SVector batch_gradient;
    for (int i=imin; i<=imax; i++) {
      const SVector &x = *(xp.at(i));
      double f =  dot(w,x) + bias;
      double y = yp.at(i);
      batch_gradient.add(x, 0.0004 * mydloss(f,y));
      batch_gradient.add(w, lambda);
      std::cout << w << std::endl;
    }
    w.add(batch_gradient,-1.0);
    batch_gradient.clear();
  }
  void do_epoch(int imin, int imax, const xvec_t& xp, const yvec_t &yp, const std::vector<int>& perm) { // this one works but not very well (slow..) attains same error as libsvm SVR on Boston
    SVector batch_gradient;
    for (int i=imin; i<=imax; i++) {
      double eta = 1.0 / (lambda * t);
      double s = 1 - eta * lambda;
      wscale *= s;
      if (wscale < 1e-9)  {
        w.scale(wscale);
        wscale = 1;
      }
      const SVector &x = *(xp.at(perm[i]));
      double y = yp.at(perm[i]);
      double wx = dot(w,x) * wscale;
      double f = wx + bias;
      double grad = mydloss(f,y) * (y<0? lossratio:1.0);
      // std::cout << "Training case " << i
      //           << " y= " << y
      //           << " f= " << f
      //           << " mydloss(f,y)= " << mydloss(f,y)
      //           << " t= " << t
      //           << " s= " << s
      //           << " grad= " << grad
      //           << " eta= " << eta
      //           << " wscale= " << wscale
      //           << " eta*grad / wscale = " << eta*grad / wscale
      //           << std::endl;
      // std::cout << w << std::endl;
      w.add(x, -eta * grad / wscale); // Note: dloss returns the true derivative so no need to multiply by -y here
      //std::cout << w << std::endl;
      t += 1;
    }
    double wnorm =  dot(w,w) * wscale * wscale;
    std::cout << std::setprecision(6) 
              << "Norm: " << wnorm << ", Bias: " << bias << std::endl;
  }
  void _orig_do_epoch(int imin, int imax, const xvec_t& xp, const yvec_t &yp) {
    SVector batch_gradient;
    for (int i=imin; i<=imax; i++) {
      //std::cout << "t= " << t << std::endl;
      double eta = 1.0 / (lambda * t);
      double s = 1 - eta * lambda;
      wscale *= s;
      if (wscale < 1e-9)  {
        w.scale(wscale);
        wscale = 1;
      }
      const SVector &x = *(xp.at(i));
      double y = yp.at(i);
      double wx = dot(w,x) * wscale;
      double f = wx + bias;
      // double z = y * (wx + bias);

      double etd = eta * mydloss(f,y) * (y<0? lossratio:1.0);
      // std::cout << "Training case " << i
      //           << " y= " << y
      //           << " f= " << f
      //           << " mydloss(f,y)= " << mydloss(f,y)
      //           << " t= " << t
      //           << " s= " << s
      //           << " etd= " << etd
      //           << " eta= " << eta
      //           << " wscale= " << wscale
      //           << " etd / wscale = " << etd / wscale
      //           << std::endl;
      //w.add(x, etd * y / wscale);
      w.add(x, etd / wscale);
      // batch_gradient.add(x, -0.001 * mydloss(f,y));
      // w.add(x, -0.000002 * mydloss(f,y));
      // std::cout << w << std::endl;
      // w.add(w, -0.000004 * lambda);
      // std::cout << w << std::endl;
#if BIAS
      // Slower rate on the bias because
      // it learns at each iteration.
      //bias += etd * y * 0.01;
#endif
      t += 1;
    }
    double wnorm =  dot(w,w) * wscale * wscale;
    std::cout << std::setprecision(6) 
              << "Norm: " << wnorm << ", Bias: " << bias << std::endl;
  }

  double get_prediction(const std::string& caseid) {
    return global_reporter.get_prediction(caseid);
  }

}; 

#endif /* LB_SCALARMODEL_H_ */




