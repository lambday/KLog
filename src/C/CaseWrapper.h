/// -*- c++ -*-
/// @file   CaseWrapper.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Wrap around various representations of sparse vectors

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



#ifndef CASEWRAPPER_H_
#define CASEWRAPPER_H_ 1
#include <iostream>
#include <stdexcept>

#ifdef linux
#include "stdlib.h"
#endif

#include "vectors.h"
#include "libsvm/svm.h"

/** Wrapper around alternative representations of feature vectors
   (different solvers use different representations)
 */

class CaseWrapper {
public:
    virtual void write(std::ostream& os) const = 0;
    virtual void set(int index, double value) = 0;

    virtual ~CaseWrapper() {
        // std::cout << "~CaseWrapper()..." << std::endl;
    };
    virtual double dot_product(CaseWrapper* other) = 0;

    CaseWrapper() {
    }
    virtual CaseWrapper* make_empty_object() = 0;
};

/** For Leon Bottou's SVector class 
 */
class LB_CaseWrapper : public CaseWrapper {
    SVector* x;
public:

    LB_CaseWrapper() {
        x = new SVector();
    }

    virtual ~LB_CaseWrapper() {
        // std::cout << "~LB_CaseWrapper()..." << std::endl;
        delete x;
    }

    virtual void write(std::ostream& os) const {
        os << *x;
    }

    virtual void set(int index, double value) {
        x->set(index, value);
    }

    virtual double dot_product(CaseWrapper* other) {
        SVector v1 = *(this->get_vector());
        SVector v2 = *(((LB_CaseWrapper*) other)->get_vector());
        return dot(v1, v2);
    }

    SVector* get_vector() const {
        return x;
    }

    CaseWrapper* make_empty_object() {
        return new LB_CaseWrapper();
    };
};


#define Malloc(type,n) (type *)malloc((n)*sizeof(type))

void malloced_add(unsigned n);
void malloced_sub(unsigned n);


// FIXME: debug with a simple driver program

class LibSVM_CaseWrapper : public CaseWrapper {
    svm_node* x;
    unsigned npairs; // stored pairs
    unsigned mpairs; // malloc'ed size

public:

    LibSVM_CaseWrapper() {
        npairs = 0;
        mpairs = 16;
        x = Malloc(svm_node, 16);
        // malloced_add(sizeof(svm_node)*16);

        x[0].index = -1;
        x[1].index = -2;
        x[1].value = 11;
        // std::cout << "constructor: x[0].index = " << x[0].index << std::endl;
        // std::cout << "constructor: x[1].index = " << x[1].index << std::endl;
    }

    virtual void write(std::ostream& os) const {
        svm_node* px = x;
        while (px->index != -1) {
            os << px->index << ":" << px->value << " ";
            ++px;
        }
    }

    svm_node* nodesearch(svm_node* x, int i) {
        int lo = 0;
        int hi = npairs - 1;
        while (lo <= hi) {
            int d = (lo + hi + 1) / 2;
            if (x[d].index == i)
                return x + d;
            else if (x[d].index == -1)
                return NULL;
            else if (i < x[d].index) {
                hi = d - 1;
            } else {
                lo = d + 1;
            }
        }
        return NULL;
    }

    void set(int i, double v) {
        if (v) {
            svm_node* p = nodesearch(x, i);
            if (p)
                // return p->value = v;
                p->value = v;
            else {
                if (npairs + 1 >= mpairs) { // +1 to make room for the last element that has index -1 according to libsvm convention..
                    // malloced_sub(sizeof(svm_node)*mpairs);
                    mpairs = 2 * mpairs;
                    x = (svm_node *) realloc(x, mpairs * sizeof (svm_node));
                    // malloced_add(sizeof(svm_node)*mpairs);
                }
                svm_node* s = x;
                p = s + npairs;
                npairs += 1;
                x[npairs].index = -1;
                // std::cout << "npairs = " << npairs << " x[npairs].index = " << x[npairs].index << std::endl;
                for (; p > s && p[-1].index > i; p--)
                    p[0] = p[-1];
                p[0].index = i;
                p[0].value = v;
            }
        } else {
            svm_node* p = nodesearch(x, i);
            if (p) { // delete old entry
                npairs -= 1;
                for (; p->index >= 0; p++)
                    p[0] = p[1];
            }
        }
        // return v;
    }

    virtual double dot_product(CaseWrapper* other) {
        svm_node *px = this->get_vector();
        svm_node *py = ((LibSVM_CaseWrapper*) other)->get_vector();
        double sum = 0;
        while (px->index != -1 && py->index != -1) {
            if (px->index == py->index) {
                sum += px->value * py->value;
                ++px;
                ++py;
            } else {
                if (px->index > py->index)
                    ++py;
                else
                    ++px;
            }
        }
        return sum;
    }

    svm_node* get_vector() const {
        return x;
    }

    virtual ~LibSVM_CaseWrapper() {
        // std::cout << "~LibSVM_CaseWrapper()..." << std::endl;
        free(x);
        // malloced_sub(sizeof(svm_node)*mpairs);
    }

    CaseWrapper* make_empty_object() {
        return new LibSVM_CaseWrapper();
    };
};


CaseWrapper* new_wrapper(const std::string& mtype);

std::ostream&
operator<<(std::ostream &os, const CaseWrapper &c);


#endif // CASEWRAPPER_H
