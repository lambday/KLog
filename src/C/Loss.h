/// -*- c++ -*-
/// @file   Loss.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Loss functions

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

#ifndef LOSS_H_
#define LOSS_H_ 1

/**
 * A loss function is simply a class with two public methods called
 * loss and dloss. No hierarchy, just add new loss classes with the
 * same public interface.  Given prediction f -- i.e. f(x) -- and
 * target y, loss should return the associated loss value and dloss
 * the derivative of the loss wrt f. The class is meant to be used as
 * a template parameter for a certain model (see classes derived from
 * Model) which in turn should contain the learning (optimization)
 * algorithm.
 */

/**
   Hinge loss (Support vector machine)
 */
struct HingeLoss {

    static bool name() {
        return "HingeLoss";
    }

    static bool ability(const std::string ab) {
        return ab == "binary_classification";
    }

    struct loss {

        double operator()(double f, double y) {
            double z = f*y;
            if (z < 1)
                return 1 - z;
            return 0;
        }
    };

    struct dloss {

        double operator()(double f, double y) {
            double z = f*y;
            if (z < 1)
                // return 1;
                return -y;
            return 0;
        }
    };
};

/**
   Log-exp or soft-hinge loss (Logistic regression)
 */
struct SoftHingeLoss {

    static std::string name() {
        return "SoftHingeLoss";
    }

    static bool ability(const std::string ab) {
        return ab == "binary_classification";
    }

    struct loss {

        double operator()(double f, double y) {
            double z = f*y;
            if (z > 18)
                return exp(-z);
            if (z < -18)
                return -z;
            return log(1 + exp(-z));
        }
    };

    struct dloss {

        double operator()(double f, double y) {
            double z = f*y;
            if (z > 18)
                // return exp(-z);
                return -y * exp(-z);
            if (z < -18)
                // return 1;
                return -y;
            // return 1 / (exp(z) + 1);
            return -y / (exp(z) + 1);
        }
    };
};

/**
   Exponential loss (Adaboost-like)
 */
// FIXME: not tested

struct ExponentialLoss {

    static std::string name() {
        return "ExponentialLoss";
    }

    static bool ability(const std::string ab) {
        return ab == "binary_classification";
    }

    struct loss {

        double operator()(double f, double y) {
            double z = f*y;
            if (z > 18)
                return 0;
            return exp(-z);
        }
    };

    struct dloss {

        double operator()(double f, double y) {
            double z = f*y;
            if (z > 18)
                return 0;
            // return -exp(-z);
            return -y * exp(-z); // FIXME: Check this!!! (was negative before changing signs)
        }
    };
};

/**
   Epsilon insensitive loss (Support vector regression)
 */
// FIXME: epsilon should be a settable flag
#define epsilon 0.01

struct EpsilonInsensitiveLoss {

    static std::string name() {
        return "EpsilonInsensitiveLoss";
    }

    static bool ability(const std::string ab) {
        return ab == "regression";
    }
    // static const double epsilon=0.01;

    static double _max(double a, double b) {
        return a > b ? a : b;
    }

    struct loss {

        double operator()(double f, double y) {
            return _max(0, fabs(y - f) - epsilon);
        }
    };

    struct dloss {

        double operator()(double f, double y) {
            return (f - y)*(fabs(y - f) > epsilon);
        }
    };
};
#undef epsilon

/**
   Square loss (Ordinary least squares regression)
 */
struct SquareLoss {

    static std::string name() {
        return "SquareLoss";
    }

    static bool ability(const std::string ab) {
        return ab == "regression" || ab == "binary_classification";
    }

    struct loss {

        double operator()(double f, double y) {
            return (f - y)*(f - y);
        }
    };

    struct dloss {

        double operator()(double f, double y) {
            return 2.0 * (f - y);
        }
    };
};

#ifdef NEVER
/**
   Softmax regression (multiclass logistic regression)
 */
// FIXME: not tested
// FIXME: unclear how to integrate this... needs something like
// LB_VectorModel.h for vector outputs. Not tested

struct SoftmaxLoss {
private:
    static double l;
    static std::vector<double> p;
    static std::vector<double> deriv;
    static bool ready;
public:

    static std::string name() {
        return "SoftmaxLoss";
    }

    static bool ability(const std::string ab) {
        return ab == "multiclass_classification";
    }

    SoftmaxLoss() {
        ready = false;
    }

    struct loss {

        double operator()(const std::vector<double>& f, const std::vector<double>& y) {
            unsigned K = f.size();
            if (K != y.size())
                throw std::length_error("size mismatch in SoftmaxLoss::loss()");
            p.resize(K);
            deriv.resize(K);
            l = 0;
            // Compute softmax. Handle overflow by subtracting max margin
            double max = f[0];
            for (unsigned k = 1; k < K; k++)
                if (f[k] > max)
                    max = f[k];
            double den = 0.0;
            for (unsigned k = 0; k < K; k++)
                den += exp(f[k] - max);
            for (unsigned k = 0; k < K; k++)
                p[k] = exp(f[k] - max) / den;
            for (unsigned k = 0; k < K; k++) {
                l += y[k] * log(p[k]);
                deriv[k] = y[k] - p[k];
            }
            ready = true;
            return l;
        }
    };

    struct dloss {

        double operator()(const std::vector<double>& f, const std::vector<double>& y, std::vector<double>& dC) {
            // Actually do nothing, it's cheaper to compute derivatives
            // while computing the loss, just retrieve the computed vector.
            if (!ready)
                throw std::logic_error("derivatives not ready in SoftmaxLoss::dloss()");
            dC = deriv;
            ready = false;
        }
    };
};

// FIXME: maybe also write Crammer & Singer loss for multiclass SVM
#endif



#endif // LOSS_H_
