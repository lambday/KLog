/// -*- c++ -*-
/// @file   c_interface.cpp
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  C-Prolog interface for kLog. Only Yap 6 currently supported

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

#include <iostream>
#include <map>
#include <vector>
#include <stdexcept>
#include <string>
#include <memory>

#include "yap.h"
#include "Histogram.h"
#include "GraphClass.h"
#include "FeatureGeneratorPool.h"
#include "wrapper.h"
#include "vectors.h"
#include "Dataset.h"
#include "ModelPool.h"
#include "FlagsService.h"
#include "kLogMaster.h"
#include "sha1.h"


// Singletons
FlagsService& The_FlagsService = FlagsService::get_instance();
FeatureGeneratorPool& The_FeatureGeneratorPool = FeatureGeneratorPool::get_instance();
Dataset& The_Dataset = Dataset::get_instance();

kLogMaster The_Master;
template<> BinaryClassifierReporter LB_ScalarModel<HingeLoss, BinaryClassifierReporter>::global_reporter = std::string("Global reporter");
template<> BinaryClassifierReporter LB_ScalarModel<SoftHingeLoss, BinaryClassifierReporter>::global_reporter = std::string("Global reporter");
template<> BinaryClassifierReporter LB_ScalarModel<ExponentialLoss, BinaryClassifierReporter>::global_reporter = std::string("Global reporter");
template<> RegressionReporter LB_ScalarModel<SquareLoss, RegressionReporter>::global_reporter = std::string("Global reporter");
template<> RegressionReporter LB_ScalarModel<EpsilonInsensitiveLoss, RegressionReporter>::global_reporter = std::string("Global reporter");

template<> BinaryClassifierReporter Libsvm_Model<BinaryClassifierReporter>::global_reporter = std::string("Global reporter");
template<> MulticlassClassifierReporter Libsvm_Model<MulticlassClassifierReporter>::global_reporter = std::string("Global reporter");
template<> RegressionReporter Libsvm_Model<RegressionReporter>::global_reporter = std::string("Global reporter");

template<> ExternalBinaryClassifierReporter External_Model<ExternalBinaryClassifierReporter>::global_reporter = std::string("Global reporter");
template<> ExternalMulticlassClassifierReporter External_Model<ExternalMulticlassClassifierReporter>::global_reporter = std::string("Global reporter");

/* ============================================================================
:- module(c_interface,[set_c_klog_flag/3,
get_c_klog_flag/3,
document_klog_c_flags/2,
klog_c_flag_client/1,
klog_c_flag_client_alt/1,
c_shasum/2,
add_graph/1,
add_graph_if_not_exists/1,
delete_graph/1,
write_graph/1,
export_graph/3,
save_as_libsvm_file/1,
cleanup_data/0,
clean_internals/1,
vertex_alive/3,
set_signature_aliveness/3,
set_slice_aliveness/3,
set_signature_in_slice_aliveness/4,
set_all_aliveness/2,
add_vertex/8,
add_edge/5,
make_sparse_vector/6,
write_sparse_vector/1,
add_feature_to_sparse_vector/3,
set_target_label/2,
set_label_name/2,
set_rejected/1,
clear_rejected/0,
remap_indices/0,
print_graph_ids/0,
new_feature_generator/2,
delete_feature_generator/1,
new_model/2,
delete_model/1,
model_type/2,
write_model/1,
load_model/2,
save_model/2,
check_model_ability/2,
train_model/2,
test_dataset/3,
dot_product/3,
set_model_wd/2,
reset_reporter/2,
reset_reporter/3,
report/2,
report/3,
save_predictions/3,
get_prediction/3
]).
============================================================================ */

/** <module> C++ interface

   General mechanism: functions listed here are the interface to
   Prolog. All model-related predicates take an atom as first argument
   that identifies the model (valid atoms should be strings, not
   integers). The identifier is then used as a key for a dictionary
   stored in the singleton The_ModelPool with associates the model
   name to a pointer to an object of Model class (Model is the
   abstract class from which all Models are inherited). Typically,
   functions defined here should call a homologous method of the class
   ModelPool that will dispatch the message to the actual model.

  ---++ Example

   Prolog code in the kLog script:

   ==
   new_model(my_model,libsvm_c_svc),
   ==

   in c_interface, c_new_model() calls the method of the singleton The_ModelPool

   ==
   The_ModelPool.new_model("my_model","libsvm_c_svc")
   ==

   which eventually creates an object of class
   Libsvm_Model<BinaryClassifierReporter>.  now back to the prolog
   code, the atom my_model can be used as a handle for referring to
   the C++ object just created, e.g.

   ==
   kfold(target_relation, 10, my_model, my_fg)
   ==

   that causes the invocation of the method

   ==
   The_ModelPool.train("my_model",vector_of_interpretation_ids)
   ==

   A similar mechanism is used for feature generators via the
   singleton The_FeatureGeneratorPool of class FeatureGeneratorPool.

  ---++ Data
   Data is contained in the singleton The_Dataset of type
   Dataset. There is a graph for each interpretation and possibly
   several sparse vectors associated to the scalar predictions (called
   cases in kLog). The former are accessed from prolog via
   interpretation identifiers, the latter via "case" identifiers.

   The_Dataset maintains internal dictionaries (indices) to
   associate identifiers to graph pointers or sparse vector pointers.

   Additionally, The_Dataset maintains a map from interpretation ids
   to a set<string> of case ids.  Use The_Dataset.get_cases() to
   retrieve this map.
 */
void moduledoc(void) {
}

// ============================================================================
//
// ****************************** Flags ***************************************
// See klog_flags.pl for a list
//
// ============================================================================

/* ============================================================================
%% set_c_klog_flag(+Client:atom,+Flag:atom,+Value:atom) is det
%
% Sets C-level kLog Flag to Value for Client
============================================================================ */
static int c_set_c_klog_flag(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        std::auto_ptr<Term> t_client(atomic_term_creator.create_term(YAP_ARG1));
        std::auto_ptr<Term> t_flag(atomic_term_creator.create_term(YAP_ARG2));
        std::auto_ptr<Term> t_value(atomic_term_creator.create_term(YAP_ARG3));
        The_FlagsService.set_flag(t_client->str(), t_flag->str(), t_value->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_c_klog_flag/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% get_c_klog_flag(+Client,+Flag,-Value) is det
%
% Unifies Value with the current value of Flag in Client.
============================================================================ */
static int c_get_c_klog_flag(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        std::auto_ptr<Term> t_client(atomic_term_creator.create_term(YAP_ARG1));
        std::auto_ptr<Term> t_flag(atomic_term_creator.create_term(YAP_ARG2));
        std::string value = The_FlagsService.get_flag(t_client->str(), t_flag->str());
        return (YAP_Unify(YAP_ARG3, YAP_MkAtomTerm(YAP_FullLookupAtom(value.c_str()))));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - get_c_klog_flag/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% document_klog_c_flags(+Client:atom,-Documentation:atom) is det
%
% Retrieve Documentation for all flags belonging to Client.
============================================================================ */
static int c_document_klog_c_flags(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        std::auto_ptr<Term> t_client(atomic_term_creator.create_term(YAP_ARG1));
        std::string doc = The_FlagsService.document_flags(t_client->str());
        return (YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_FullLookupAtom(doc.c_str()))));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - document_klog_c_flags/1): " << e.what() << std::endl;
        return 0;
    }
}


// ============================================================================

typedef struct {
    vector<string> * client_ids;
} preserved_klog_flag_client_type;

/* ============================================================================
%% klog_c_flag_client(?Client) is nondet
%
% Backtrack over the IDs of the C language flag clients.
============================================================================ */
static int continue_klog_flag_client(void) {
    try {
        preserved_klog_flag_client_type *preserved_klog_flag_client;
        YAP_Term sol = YAP_ARG1;
        YAP_PRESERVED_DATA(preserved_klog_flag_client, preserved_klog_flag_client_type);
        string an_id = preserved_klog_flag_client->client_ids->back();
        preserved_klog_flag_client->client_ids->pop_back();
        YAP_Term id_term = YAP_MkAtomTerm(YAP_FullLookupAtom(an_id.c_str()));
        YAP_Unify(sol, id_term);
        if (preserved_klog_flag_client->client_ids->empty()) {
            YAP_cut_succeed();
        }
        return (TRUE);
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - cont klog_flag_client/1): " << e.what() << std::endl;
        return 0;
    }
}

static int start_klog_flag_client(void) {
    try {
        preserved_klog_flag_client_type *preserved_klog_flag_client;
        AtomicTermCreator atomic_term_creator;
        YAP_Term c = YAP_ARG1;
        YAP_PRESERVE_DATA(preserved_klog_flag_client, preserved_klog_flag_client_type);
        if (YAP_IsVarTerm(c)) {
            preserved_klog_flag_client->client_ids = new std::vector<std::string>();
            The_FlagsService.get_client_ids(*(preserved_klog_flag_client->client_ids));
            if (preserved_klog_flag_client->client_ids->empty()) {
                YAP_cut_fail();
                return 1;
            } else {
                return continue_klog_flag_client();
            }
        }
        std::auto_ptr<Term> t_client(atomic_term_creator.create_term(YAP_ARG1));
        bool is_client = The_FlagsService.is_registered_client(t_client->str());
        if (is_client) {
            YAP_cut_succeed();
        } else {
            YAP_cut_fail();
        }
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - klog_flag_client/1): " << e.what() << std::endl;
        return 0;
    }
}

static int cut_klog_flag_client(void) {
    try {
        preserved_klog_flag_client_type *preserved_klog_flag_client;
        YAP_Term c = YAP_ARG1;
        if (YAP_IsVarTerm(c)) {
            YAP_PRESERVED_DATA(preserved_klog_flag_client, preserved_klog_flag_client_type);
            delete preserved_klog_flag_client->client_ids;
        }
        return (TRUE);
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - cut klog_flag_client/1): " << e.what() << std::endl;
        return 0;
    }
}

// ============================================================================

typedef struct {
    FlagsService::Map::const_iterator it_begin;
    FlagsService::Map::const_iterator it_end;
} preserved_klog_flag_client_alt_type;

/* ============================================================================
%% klog_c_flag_client_alt(?Client) is nondet
%
% Backtrack over the IDs of the C language flag clients.
% This implementation uses less memory, it doesn't store a copy of all alternatives in memory
============================================================================ */
static int continue_klog_flag_client_alt(void) {
    try {
        preserved_klog_flag_client_alt_type *preserved_klog_flag_client_alt;
        YAP_Term sol = YAP_ARG1;
        YAP_PRESERVED_DATA(preserved_klog_flag_client_alt, preserved_klog_flag_client_alt_type);
        string an_id = preserved_klog_flag_client_alt->it_begin->first;

        preserved_klog_flag_client_alt->it_begin++;
        YAP_Term id_term = YAP_MkAtomTerm(YAP_FullLookupAtom(an_id.c_str()));
        YAP_Unify(sol, id_term);
        if (preserved_klog_flag_client_alt->it_begin == preserved_klog_flag_client_alt->it_end) {
            YAP_cut_succeed();
        }
        return (TRUE);
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - cont klog_flag_client_alt/1): " << e.what() << std::endl;
        return 0;
    }
}

static int start_klog_flag_client_alt(void) {
    try {
        preserved_klog_flag_client_alt_type *preserved_klog_flag_client_alt;
        AtomicTermCreator atomic_term_creator;
        YAP_Term c = YAP_ARG1;
        YAP_PRESERVE_DATA(preserved_klog_flag_client_alt, preserved_klog_flag_client_alt_type);
        if (YAP_IsVarTerm(c)) {
            //preserved_klog_flag_client_alt->client_ids = new std::vector<std::string>();
            The_FlagsService.get_client_ids_alt(preserved_klog_flag_client_alt->it_begin, preserved_klog_flag_client_alt->it_end);
            if (preserved_klog_flag_client_alt->it_begin == preserved_klog_flag_client_alt->it_end) {
                YAP_cut_fail();
                return 1;
            } else {
                return continue_klog_flag_client_alt();
            }
        }
        std::auto_ptr<Term> t_client(atomic_term_creator.create_term(YAP_ARG1));
        bool is_client = The_FlagsService.is_registered_client(t_client->str());
        if (is_client) {
            YAP_cut_succeed();
        } else {
            YAP_cut_fail();
        }
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - klog_flag_client_alt/1): " << e.what() << std::endl;
        return 0;
    }
}

static int cut_klog_flag_client_alt(void) {
    return (TRUE);
}
// ============================================================================
//
// ****************************** Utility**************************************
//
// ============================================================================

/* ============================================================================
%% c_shasum(+Filename,-HashValue) is det
%
% Unifies HashValue with the SHA1 hash value of the file (which must exist).
============================================================================ */
static int c_shasum(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        std::auto_ptr<Term> t_filename(atomic_term_creator.create_term(YAP_ARG1));
        std::string hashvalue;

        SHA1Context sha; // SHA-1 context
        FILE *fp; // File pointer for reading files
        unsigned char c; // Character read from file

        SHA1Reset(&sha);

        if (!(fp = fopen(t_filename->str().c_str(), "rb"))) {
            throw std::runtime_error("shasum: unable to open file " + t_filename->str());
        }

        c = fgetc(fp);
        while (!feof(fp)) {
            SHA1Input(&sha, &c, 1);
            c = fgetc(fp);
        }
        fclose(fp);

        if (!SHA1Result(&sha)) {
            throw std::runtime_error("shasum: could not compute shasum for " + t_filename->str());
        }
        std::ostringstream os;
        for (int i = 0; i < 5; ++i) {
            os << std::hex << sha.Message_Digest[i];
        }
        hashvalue = os.str();

        return (YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_FullLookupAtom(hashvalue.c_str()))));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - c_shasum/2): " << e.what() << std::endl;
        return 0;
    }
}


// ============================================================================
//
// ******************* Dataset related predicates *****************************
//
// ============================================================================

// ============================================================================

/* ============================================================================
%% add_graph(+Id) is det
%
% Create a new graph identified by Id. Id can be any valid Prolog
% atom.
============================================================================ */
static int c_add_graph(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        std::auto_ptr<Term> t_gid(atomic_term_creator.create_term(arg_gid));
        The_Dataset.add_graph(t_gid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - add_graph/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% add_graph_if_not_exists(+Id) is det
%
% Create a new graph identified by Id unless it already exists. Id
% can be any valid Prolog atom
============================================================================ */
static int c_add_graph_if_not_exists(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        std::auto_ptr<Term> t_gid(atomic_term_creator.create_term(arg_gid));
        The_Dataset.add_graph_if_not_exists(t_gid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - add_graph_if_not_exists/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% delete_graph(+Id) is det
%
% Delete graph identified by Id.
============================================================================ */
static int c_delete_graph(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        std::auto_ptr<Term> t_gid(atomic_term_creator.create_term(arg_gid));
        The_Dataset.delete_graph(t_gid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - delete_graph/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% write_graph(+Id) is det
%
% Write graph identified by Id on the standard output.
============================================================================ */
static int c_write_graph(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        std::auto_ptr<Term> t_gid(atomic_term_creator.create_term(arg_gid));
        The_Dataset.write_graph(t_gid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - write_graph/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% export_graph(+Id,+Filename,+Format)
%
% Save graph identified by Id into Filename in specified
% Format. File extension is automatically created from format (it is
% actually identical). Valid formats are dot, csv, gml, gdl, gspan.
============================================================================ */
static int c_export_graph(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        YAP_Term arg_filename = YAP_ARG2;
        YAP_Term arg_format = YAP_ARG3;
        std::auto_ptr<Term> t_gid(atomic_term_creator.create_term(arg_gid));
        std::auto_ptr<Term> t_filename(atomic_term_creator.create_term(arg_filename));
        std::auto_ptr<Term> t_format(atomic_term_creator.create_term(arg_format));
        The_Dataset.export_graph(t_gid->str(), t_filename->str(), t_format->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - export_graph/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% save_as_libsvm_file(+Filename) is det
%
% Save a sparse vector dataset into Filename in libsvm format. Data
% is generated for all interpretations in the attached dataset. Every
% line is a case. The line is ended by a comment giving the case
% name, which is formed by concatenating the interpretation name and
% the identifiers contained in the target fact.
============================================================================ */
static int c_save_as_libsvm_file() {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_filename = YAP_ARG1;
        std::auto_ptr<Term> t_filename(atomic_term_creator.create_term(arg_filename));
        The_Dataset.save_as_libsvm_file(t_filename->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - save_as_libsvm_file/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% cleanup_data is det
%
% Clean completely graphs and sparse vectors.
============================================================================ */
static int c_cleanup_data(void) {
    try {
        The_Dataset.cleanup_data();
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - cleanup_data/0): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% clean_internals(+GId) is det
%
% Clean up internal data structures on GId to recover memory
============================================================================ */
static int c_clean_internals(void) {
    try {
        AtomTermCreator atom_term_creator;
        IntTermCreator int_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        GraphClass* g = The_Dataset.get_graph(t_gid->str());
        g->clean_internals();
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - clean_internals/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% vertex_alive(+GId,+VId,?State) is det
%
% Set/get aliveness State (yes/no) of vertex VId in graph GId
============================================================================ */
static int c_vertex_alive(void) {
    try {
        AtomTermCreator atom_term_creator;
        IntTermCreator int_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        YAP_Term arg_vid = YAP_ARG2;
        YAP_Term arg_is_alive = YAP_ARG3;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        std::auto_ptr<IntTerm> t_vid(int_term_creator.create_term(arg_vid));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());
        if (YAP_IsVarTerm(arg_is_alive) == TRUE) { // Uninstantiated variable
            YAP_Term t_alive;
            if (current_graph->GetVertexAlive(t_vid->get_int()))
                t_alive = YAP_MkAtomTerm(YAP_FullLookupAtom("yes"));
            else
                t_alive = YAP_MkAtomTerm(YAP_FullLookupAtom("no"));
            return (YAP_Unify(arg_is_alive, t_alive));
        } else {
            AtomTermCreator atom_term_creator;
            std::auto_ptr<AtomTerm> t_is_alive(atom_term_creator.create_term(arg_is_alive));
            bool c_is_alive = (t_is_alive->str() == "yes");
            current_graph->SetVertexAlive(t_vid->get_int(), c_is_alive);
            return 1;
        }
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - vertex_alive/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_signature_aliveness(+GId,+S,+State) is det
%
% Set aliveness state to State to every vertex of signature S in graph GId
============================================================================ */
static int c_set_signature_aliveness(void) {
    try {
        AtomTermCreator atom_term_creator;
        IntTermCreator int_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        YAP_Term arg_sig = YAP_ARG2;
        YAP_Term arg_state = YAP_ARG3;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        std::auto_ptr<AtomTerm> t_sig(atom_term_creator.create_term(arg_sig));
        std::auto_ptr<AtomTerm> t_state(atom_term_creator.create_term(arg_state));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());
        bool state = t_state->str() == "yes" || t_state->str() == "true";
        current_graph->SetSignatureAliveness(t_sig->str(), state);
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_signature_aliveness/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_slice_aliveness(+GId,+Slice,+State) is det
%
% Set aliveness state to Status to every vertex in slice Slice in graph GId
============================================================================ */
static int c_set_slice_aliveness(void) {
    try {
        AtomTermCreator atom_term_creator;
        IntTermCreator int_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        YAP_Term arg_slice = YAP_ARG2;
        YAP_Term arg_state = YAP_ARG3;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        std::auto_ptr<AtomTerm> t_slice(atom_term_creator.create_term(arg_slice));
        std::auto_ptr<AtomTerm> t_state(atom_term_creator.create_term(arg_state));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());
        bool state = t_state->str() == "yes" || t_state->str() == "true";
        current_graph->SetSliceAliveness(t_slice->str(), state);
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_slice_aliveness/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_signature_in_slice_aliveness(+GId,+S,+SliceID,+State) is det
%
% Set aliveness state to State to every vertex of signature S in
% slice SliceID in graph GId
============================================================================ */
static int c_set_signature_in_slice_aliveness(void) {
    try {
        AtomTermCreator atom_term_creator;
        IntTermCreator int_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        YAP_Term arg_sig = YAP_ARG2;
        YAP_Term arg_slice = YAP_ARG3;
        YAP_Term arg_state = YAP_ARG4;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        std::auto_ptr<AtomTerm> t_sig(atom_term_creator.create_term(arg_sig));
        std::auto_ptr<AtomTerm> t_slice(atom_term_creator.create_term(arg_slice));
        std::auto_ptr<AtomTerm> t_state(atom_term_creator.create_term(arg_state));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());
        bool state = t_state->str() == "yes" || t_state->str() == "true";
        current_graph->SetSignatureInSliceAliveness(t_sig->str(), t_slice->str(), state);
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_signature_in_slice_aliveness/4): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_all_aliveness(+GId,+State) is det
%
% Set aliveness state to State in every vertex in graph GId
============================================================================ */
static int c_set_all_aliveness(void) {
    try {
        AtomTermCreator atom_term_creator;
        IntTermCreator int_term_creator;
        YAP_Term arg_gid = YAP_ARG1;
        YAP_Term arg_state = YAP_ARG2;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        std::auto_ptr<AtomTerm> t_state(atom_term_creator.create_term(arg_state));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());
        bool state = t_state->str() == "yes" || t_state->str() == "true";
        current_graph->SetAllAliveness(state);
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_all_aliveness/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% add_vertex(+GId,+SliceId,+Label,+IsKernelPoint,+IsAlive,+Kind,+SymId,-Id) is det
%
% Create new vertex in graph GId with label Label and unify Id with
% the identifier of the new vertex Set KernelPoint to 'yes' to
% indicate that this vertex is a kernel point Set IsAlive to 'yes'
% to indicate that this vertex is alive (i.e. actually exists). Dead
% vertices are used for structured output predictions.  Kind should
% be either 'r' for relationship or 'i' for an entity set. SymId is
% the symbolic id of the vertex and is only used for
% visualization/debugging purposes. SliceId is the slice to which
% this vertex belongs to.
============================================================================ */
static int c_add_vertex(void) {
    YAP_Term arg_gid = YAP_ARG1;
    YAP_Term arg_sliceid = YAP_ARG2;
    YAP_Term arg_label = YAP_ARG3;
    YAP_Term arg_kernel_point = YAP_ARG4;
    YAP_Term arg_is_alive = YAP_ARG5;
    YAP_Term arg_kind = YAP_ARG6;
    YAP_Term arg_symid = YAP_ARG7;
    YAP_Term arg_id = YAP_ARG8;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());
        std::auto_ptr<AtomTerm> t_sliceid(atom_term_creator.create_term(arg_sliceid));
        PrologNumericalListCreator prolog_numerical_list_creator;
        std::auto_ptr<PrologNumericalList> num_list(prolog_numerical_list_creator.create_term(arg_label));

        PrologAtomListCreator prolog_atom_list_creator;
        std::auto_ptr<PrologAtomList> atom_list(prolog_atom_list_creator.create_term(arg_label));
        // Laura TODO:
        // PrologRealvectorCreator prolog_realvector_list_creator;
        // std::auto_ptr<PrologRealvectorList> realv_list(prolog_realvector_list_creator.create_term(arg_label));

        std::auto_ptr<AtomTerm> t_kernel_point(atom_term_creator.create_term(arg_kernel_point));
        bool c_kernel_point = (t_kernel_point->str() == "yes");
        std::auto_ptr<AtomTerm> t_is_alive(atom_term_creator.create_term(arg_is_alive));
        bool c_is_alive = (t_is_alive->str() == "yes");

        std::auto_ptr<AtomTerm> t_kind(atom_term_creator.create_term(arg_kind));
        bool c_kind = t_kind->str()[0] == 'i' ? true : false;

        std::auto_ptr<AtomTerm> t_symid(atom_term_creator.create_term(arg_symid));

        // int c_id = current_graph->add_vertex(num_list->get_list(),atom_list->get_list(), c_kernel_point, c_is_alive, c_kind);
        unsigned c_id = current_graph->InsertVertex();
        current_graph->SetSliceID(c_id, t_sliceid->str());
        current_graph->SetVertexNumericAttributeList(c_id, num_list->get_list());
        current_graph->SetVertexSymbolicAttributeList(c_id, atom_list->get_list());
        // Laura TODO:
        // current_graph->SetVertexRealvectorAttributeList(c_id,realv_list->get_list());
        current_graph->SetVertexKernelPoint(c_id, c_kernel_point);
        current_graph->SetVertexAlive(c_id, c_is_alive);
        current_graph->SetVertexKind(c_id, c_kind);
        current_graph->SetVertexSymbolicID(c_id, t_symid->str());
        YAP_Term t_id = YAP_MkIntTerm(c_id);
        return (YAP_Unify(arg_id, t_id));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - add_vertex/6): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% add_edge(+GId,+V,+U,+Label, -Id) is det
%
% Create new edge between U and V with label Label and unify Id with
% the identifier of the new edge
============================================================================ */
static int c_add_edge(void) {
    YAP_Term arg_gid = YAP_ARG1;
    YAP_Term arg_u = YAP_ARG2;
    YAP_Term arg_v = YAP_ARG3;
    YAP_Term arg_label = YAP_ARG4;
    YAP_Term arg_id = YAP_ARG5;
    try {
        IntTermCreator int_term_creator;
        std::auto_ptr<IntTerm> t_u(int_term_creator.create_term(arg_u));
        std::auto_ptr<IntTerm> t_v(int_term_creator.create_term(arg_v));

        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_gid(atom_term_creator.create_term(arg_gid));
        GraphClass* current_graph = The_Dataset.get_graph(t_gid->str());

        PrologNumericalListCreator prolog_numerical_list_creator;
        std::auto_ptr<PrologNumericalList> num_list(prolog_numerical_list_creator.create_term(arg_label));
        PrologAtomListCreator prolog_atom_list_creator;
        std::auto_ptr<PrologAtomList> atom_list(prolog_atom_list_creator.create_term(arg_label));
        int c_id = current_graph->InsertEdge(t_u->get_int(), t_v->get_int());
        current_graph->SetEdgeNumericAttributeList(c_id, num_list->get_list());
        current_graph->SetEdgeSymbolicAttributeList(c_id, atom_list->get_list());
        c_id = current_graph->InsertEdge(t_v->get_int(), t_u->get_int());
        current_graph->SetEdgeNumericAttributeList(c_id, num_list->get_list());
        current_graph->SetEdgeSymbolicAttributeList(c_id, atom_list->get_list());
        YAP_Term t_id = YAP_MkIntTerm(c_id);
        return (YAP_Unify(arg_id, t_id));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - add_edge/5): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% make_sparse_vector(+ModelType:atom,+FG:atom,+IntId:atom,+SliceId,+CaseId:atom,+ViewPoints:list_of_numbers) is det
%
% Use feature generator FG to make a sparse vector from graph Ex and
% focusing on the list of vertices in ViewPoints. The sparse vector
% is then identified by CaseId. ModelType is used to construct an
% appropriate internal representation of the feature vector.
============================================================================ */
static int c_make_sparse_vector(void) {
    YAP_Term arg_model_type = YAP_ARG1;
    YAP_Term arg_fg = YAP_ARG2;
    YAP_Term arg_int_id = YAP_ARG3;
    YAP_Term arg_slice_id = YAP_ARG4;
    YAP_Term arg_case_id = YAP_ARG5;
    YAP_Term arg_viewpoints = YAP_ARG6;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_model_type(atom_term_creator.create_term(arg_model_type));
        std::auto_ptr<AtomTerm> t_fg(atom_term_creator.create_term(arg_fg));
        std::auto_ptr<AtomTerm> t_int_id(atom_term_creator.create_term(arg_int_id));
        std::auto_ptr<AtomTerm> t_slice_id(atom_term_creator.create_term(arg_slice_id));
        std::auto_ptr<AtomTerm> t_case_id(atom_term_creator.create_term(arg_case_id));
        PrologNumericalListCreator prolog_numerical_list_creator;
        std::auto_ptr<PrologNumericalList> t_viewpoints(prolog_numerical_list_creator.create_term(arg_viewpoints));
        The_Dataset.make_sparse_vector(t_model_type->str(), t_fg->str(), t_int_id->str(), t_slice_id->str(), t_case_id->str(), t_viewpoints->get_list());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - make_sparse_vector/6): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% write_sparse_vector(+CaseId) is det
%
% Write the sparse vector identified by CaseId.
============================================================================ */
static int c_write_sparse_vector(void) {
    YAP_Term arg_case_id = YAP_ARG1;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_case_id(atom_term_creator.create_term(arg_case_id));
        The_Dataset.write_sparse_vector(t_case_id->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - write_sparse_vector/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% add_feature_to_sparse_vector(+CaseId,+FeatureStr,+FeatureVal) is det
%
% Adds new feature to sparse vector identified by CaseId. FeatureStr
% is hashed to produce the index in the sparse vector. FeatureVal is
% then assigned to vector[hash(FeatureStr)]
% This method was introduced for ghost signatures
============================================================================ */
static int c_add_feature_to_sparse_vector(void) {
    YAP_Term arg_case_id = YAP_ARG1;
    YAP_Term arg_feature_str = YAP_ARG2;
    YAP_Term arg_feature_val = YAP_ARG3;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_case_id(atom_term_creator.create_term(arg_case_id));
        std::auto_ptr<AtomTerm> t_feature_str(atom_term_creator.create_term(arg_feature_str));
        FloatTermCreator float_term_creator;
        std::auto_ptr<FloatTerm> t_feature_val(float_term_creator.create_term(arg_feature_val));
        The_Dataset.add_feature_to_sparse_vector(t_case_id->str(), t_feature_str->str(), t_feature_val->get_float());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - add_feature_to_sparse_vector/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_target_label(+CaseID,+Label) is det
%
% Set the label for SVM CaseID
============================================================================ */
static int c_set_target_label(void) {
    YAP_Term arg_case_id = YAP_ARG1;
    YAP_Term arg_target_label = YAP_ARG2;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_case_id(atom_term_creator.create_term(arg_case_id));
        FloatTermCreator float_term_creator;
        std::auto_ptr<FloatTerm> t_target_label(float_term_creator.create_term(arg_target_label));
        The_Dataset.set_target_label(t_case_id->str(), t_target_label->get_float());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_target_label/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_label_name(+Label:atom,+NumericLabel:number) is det
%
% Record label name
============================================================================ */
static int c_set_label_name(void) {
    YAP_Term arg_label = YAP_ARG1;
    YAP_Term arg_numeric_label = YAP_ARG2;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_label(atom_term_creator.create_term(arg_label));
        FloatTermCreator float_term_creator;
        std::auto_ptr<FloatTerm> t_numeric_label(float_term_creator.create_term(arg_numeric_label));
        The_Dataset.set_label_name(t_label->str(), t_numeric_label->get_float());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_label_name/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% set_rejected(+CaseID) is det
%
% Set reject flag for SVM CaseID
============================================================================ */
static int c_set_rejected(void) {
    YAP_Term arg_case_id = YAP_ARG1;
    try {
        AtomTermCreator atom_term_creator;
        std::auto_ptr<AtomTerm> t_case_id(atom_term_creator.create_term(arg_case_id));
        The_Dataset.set_rejected(t_case_id->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_rejected/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% clear_rejected is det
%
% Clear all reject flags
============================================================================ */
static int c_clear_rejected(void) {
    try {
        The_Dataset.clear_rejected();
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - clear_rejected/0): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% remap_indices is det
%
% Remap feature vector indices in a small range
============================================================================ */
static int c_remap_indices(void) {
    try {
        The_Dataset.remap_indices();
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - remap_indices/0): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% print_graph_ids is det
%
% Print all IDs for graphs in the dataset
============================================================================ */
static int c_print_graph_ids(void) {
    try {
        The_Dataset.print_graph_ids();
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - print_graph_ids/0): " << e.what() << std::endl;
        return 0;
    }
}



// ============================================================================
//
// ******************** Feature generator predicates **************************
// 
// ============================================================================

/* ============================================================================
%% new_feature_generator(+FGId,+FGType) is det
%
% Create a new feature generator identified by atom FGId
============================================================================ */
static int c_new_feature_generator(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_fgid = YAP_ARG1;
        YAP_Term arg_fgtype = YAP_ARG2;
        std::auto_ptr<Term> t_fgid(atomic_term_creator.create_term(arg_fgid));
        std::auto_ptr<Term> t_fgtype(atomic_term_creator.create_term(arg_fgtype));
        The_FeatureGeneratorPool.new_feature_generator(t_fgid->str(), t_fgtype->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - new_feature_generator/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% delete_feature_generator(+Id) is det
%
% Delete feature_generator identified by Id.
============================================================================ */
static int c_delete_feature_generator(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_fgid = YAP_ARG1;
        std::auto_ptr<Term> t_fgid(atomic_term_creator.create_term(arg_fgid));
        The_FeatureGeneratorPool.delete_feature_generator(t_fgid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - delete_feature_generator/1): " << e.what() << std::endl;
        return 0;
    }
}

// ============================================================================
//
// ******************** Model related predicates ******************************
//
// As a general rule, models, interpretations, and cases in this
// section are only identified by strings (which correspond to prolog
// atoms), not their pointers. The conversion happens in Pool.h
// 
// ============================================================================

/* ============================================================================
%% new_model(+ModelId,+ModelType) is det
%
% Create a new model identified by atom ModelId
============================================================================ */
static int c_new_model(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_mtype = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_mtype(atomic_term_creator.create_term(arg_mtype));
        The_ModelPool.new_model(t_mid->str(), t_mtype->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - new_model/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% delete_model(+Id) is det
%
% Delete model identified by Id.
============================================================================ */
static int c_delete_model(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        The_ModelPool.delete_model(t_mid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - delete_model/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% model_type(+ModelId,-ModelType) is det
%
% Unifies ModelType with type of model identified by ModelId.
============================================================================ */
static int c_model_type(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(YAP_ARG1));
        std::string model_type = The_ModelPool.get_model_type(t_mid->str());
        return (YAP_Unify(YAP_ARG2, YAP_MkAtomTerm(YAP_FullLookupAtom(model_type.c_str()))));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - model_type/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% write_model(+Id) is det
%
% Write model identified by Id.
============================================================================ */
static int c_write_model(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        The_ModelPool.write_model(t_mid->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - write_model/1): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% load_model(+Id,+Filename) is det
%
% Load model identified by Id from Filename
============================================================================ */
static int c_load_model(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_filename = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_filename(atomic_term_creator.create_term(arg_filename));
        The_ModelPool.load_model(t_mid->str(), t_filename->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - load_model/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% save_model(+Id,+Filename) is det
%
% Save model identified by Id into Filename in kLog internal format
============================================================================ */
static int c_save_model(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_filename = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_filename(atomic_term_creator.create_term(arg_filename));
        The_ModelPool.save_model(t_mid->str(), t_filename->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - save_model/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% check_model_ability(+ModelId:atom,+Ability:atom) is det
%
% Ask ModelId whether it can perform the task specified in Ability.
============================================================================ */
static int c_check_model_ability(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_ability = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_ability(atomic_term_creator.create_term(arg_ability));
        if (The_ModelPool.check_model_ability(t_mid->str(), t_ability->str()))
            return 1;
        else {
            std::cerr << "Model >" << t_mid->str() << "< has not the required ability >"
                    << t_ability->str() << "<" << std::endl;
            return 0;
        }
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - check_model_ability/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% train_model(+ModelId,+DatasetSpec) is det
%
% Train model identified by ModelIdId. The list DatasetSpec should
% contain identifiers of interpretations. Case identifiers are then
% generated in Pool.h
============================================================================ */
static int c_train_model(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        PrologAtomListCreator prolog_atom_list_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_dataset = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<PrologAtomList> t_dataset(prolog_atom_list_creator.create_term(arg_dataset));
        The_ModelPool.train_model(t_mid->str(), t_dataset->get_list());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - train_model/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================
%% test_dataset(+ModelId:atom,+DatasetSpec:atom,+NewData:atom) is det
%
% Test model identified by ModelIdId on a dataset. The list
% DatasetSpec should contain identifiers of interpretations. Case
% identifiers are then generated in Pool.h. If NewData is "true" (or
% "yes") then it is assumed that the evaluation is on test data and
% results are accumulated in the global reporter of the
% model. Otherwise it is assumed that we are testing on training data
% and only the local reporter is affected.

============================================================================ */
static int c_test_dataset(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        PrologAtomListCreator prolog_atom_list_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_dataset = YAP_ARG2;
        YAP_Term arg_newdata = YAP_ARG3;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<PrologAtomList> t_dataset(prolog_atom_list_creator.create_term(arg_dataset));
        std::auto_ptr<Term> t_newdata(atomic_term_creator.create_term(arg_newdata));
        bool newdata = (t_newdata->str() == "true" || t_newdata->str() == "yes");
        The_ModelPool.test_dataset(t_mid->str(), t_dataset->get_list(), newdata);
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - test_dataset/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% dot_product(+CaseId1,+CaseId2,-Value) is det
%
%
% Unify Value with the dot product of the feature vectors attached to
% case identifiers CaseId1 and CaseId2.
============================================================================ */

static int c_dot_product(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_caseid1 = YAP_ARG1;
        YAP_Term arg_caseid2 = YAP_ARG2;
        YAP_Term arg_value = YAP_ARG3;
        std::auto_ptr<Term> t_caseid1(atomic_term_creator.create_term(arg_caseid1));
        std::auto_ptr<Term> t_caseid2(atomic_term_creator.create_term(arg_caseid2));
        double value = The_Dataset.dot_product(t_caseid1->str(), t_caseid2->str());
        YAP_Term t_value = YAP_MkFloatTerm(value);
        return (YAP_Unify(arg_value, t_value));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - dot_product/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% set_model_wd(+ModelId,+WorkingDir) is det
%
% Inform model of the working directory. This is mainly needed for
%  external models in order to keep a clean filesystem.

============================================================================ */

static int c_set_model_wd(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_wd = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_wd(atomic_term_creator.create_term(arg_wd));
        The_ModelPool.set_model_wd(t_mid->str(), t_wd->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - set_model_wd/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% reset_reporter(+ModelId,+Reporter) is det
%
% Reset a reporter of the model. Reporter should be either local or
% global. 
============================================================================ */

static int c_reset_reporter(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_rep = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_rep(atomic_term_creator.create_term(arg_rep));
        The_ModelPool.reset_reporter(t_mid->str(), t_rep->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - reset_reporter/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% reset_reporter(+ModelId,+Reporter,+Description) is det
%
% Reset a reporter of the model. Reporter should be either local or
% global. Also sets the description message to Description.
============================================================================ */
static int c_reset_reporter3(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_rep = YAP_ARG2;
        YAP_Term arg_desc = YAP_ARG3;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_rep(atomic_term_creator.create_term(arg_rep));
        std::auto_ptr<Term> t_desc(atomic_term_creator.create_term(arg_desc));
        The_ModelPool.reset_reporter(t_mid->str(), t_rep->str(), t_desc->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - reset_reporter/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% report(+ModelId,+Reporter) is det
%
% Ask a performance reporter to write a report on the
% standard output. Reporter should be either local or global.
============================================================================ */

static int c_report(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_rep = YAP_ARG2;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_rep(atomic_term_creator.create_term(arg_rep));
        The_ModelPool.report(t_mid->str(), std::cout, t_rep->str()); // use standard output
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - report/2): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% report(+ModelId,+Filename,+Reporter) is det
%
% Ask a performance reporter to write a report to Filename. Reporter
% should be either local or global.
============================================================================ */

static int c_report3(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_filename = YAP_ARG2;
        YAP_Term arg_rep = YAP_ARG3;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_filename(atomic_term_creator.create_term(arg_filename));
        std::auto_ptr<Term> t_rep(atomic_term_creator.create_term(arg_rep));
        std::ofstream os(t_filename->str().c_str());
        The_ModelPool.report(t_mid->str(), os, t_rep->str());
        os.close();
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - report/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% save_predictions(+ModelId,+Dir,+Reporter) is det
%
% Save predictions stored in a performance reporter in output.log
% and maybe output.yyy in Dir. Do AUC analysis for binary
% classification reporters. Reporter should be either local or global.
============================================================================ */

static int c_save_predictions(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_dir = YAP_ARG2;
        YAP_Term arg_rep = YAP_ARG3;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_dir(atomic_term_creator.create_term(arg_dir));
        std::auto_ptr<Term> t_rep(atomic_term_creator.create_term(arg_rep));
        The_ModelPool.save_predictions(t_mid->str(), t_dir->str(), t_rep->str());
        return 1;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - save_predictions/3): " << e.what() << std::endl;
        return 0;
    }
}

/* ============================================================================

%% get_prediction(+ModelId,+CaseID,-Margin) is det
%
% Obtain prediction for CaseID in the global reporter of model
% identified by ModelId. WARNING: fails without displaying any message
% if arguments are not valid.

============================================================================ */

static int c_get_prediction(void) {
    try {
        AtomicTermCreator atomic_term_creator;
        YAP_Term arg_mid = YAP_ARG1;
        YAP_Term arg_caseid = YAP_ARG2;
        YAP_Term arg_margin = YAP_ARG3;
        std::auto_ptr<Term> t_mid(atomic_term_creator.create_term(arg_mid));
        std::auto_ptr<Term> t_caseid(atomic_term_creator.create_term(arg_caseid));
        double margin = The_ModelPool.get_prediction(t_mid->str(), t_caseid->str());
        YAP_Term t_margin = YAP_MkFloatTerm(margin);
        return (YAP_Unify(arg_margin, t_margin));
    } catch (std::exception & e) {
        std::cerr << "Exception (C++ - get_prediction/3): " << e.what() << std::endl;
        return 0;
    }
}



// TODO: set model parameters, possibly as
// model_parameter(+Model,+ParamName,+ParamValue)

// ============================================================================

extern "C" { // avoid C++ name mangling or load_foreign_files/3 will fail!

    void c_init() {

        // Flags related predicates
        YAP_UserCPredicate("set_c_klog_flag", c_set_c_klog_flag, 3);
        YAP_UserCPredicate("get_c_klog_flag", c_get_c_klog_flag, 3);
        YAP_UserCPredicate("document_klog_c_flags", c_document_klog_c_flags, 2);
        // demonstration of a nondet predicate which preserves data between backtracking calls
        YAP_UserBackCutCPredicate((char*) "c_klog_flag_client",
                // Not sure why this one gives a warning (YAP_UserCPredicate won't) but the cast placates g++ 4.2.1
                start_klog_flag_client, continue_klog_flag_client, cut_klog_flag_client,
                1, sizeof (preserved_klog_flag_client_type));
        // alternative implementation
        YAP_UserBackCutCPredicate((char*) "c_klog_flag_client_alt",
                start_klog_flag_client_alt, continue_klog_flag_client_alt, cut_klog_flag_client_alt,
                1, sizeof (preserved_klog_flag_client_alt_type));
        // Utility
        YAP_UserCPredicate("shasum", c_shasum, 2);

        // Data related predicates
        YAP_UserCPredicate("add_graph", c_add_graph, 1);
        YAP_UserCPredicate("add_graph_if_not_exists", c_add_graph_if_not_exists, 1);
        YAP_UserCPredicate("delete_graph", c_delete_graph, 1);
        YAP_UserCPredicate("write_graph", c_write_graph, 1);
        YAP_UserCPredicate("export_graph", c_export_graph, 3);
        YAP_UserCPredicate("save_as_libsvm_file", c_save_as_libsvm_file, 1);
        YAP_UserCPredicate("cleanup_data", c_cleanup_data, 0);
        YAP_UserCPredicate("clean_internals", c_clean_internals, 1);

        // YAP_UserCPredicate("set_y",c_set_y,2);
        YAP_UserCPredicate("vertex_alive", c_vertex_alive, 2);

        YAP_UserCPredicate("set_signature_aliveness", c_set_signature_aliveness, 3);
        YAP_UserCPredicate("set_slice_aliveness", c_set_slice_aliveness, 3);
        YAP_UserCPredicate("set_signature_in_slice_aliveness", c_set_signature_in_slice_aliveness, 4);
        YAP_UserCPredicate("set_all_aliveness", c_set_all_aliveness, 2);

        YAP_UserCPredicate("add_vertex", c_add_vertex, 8);
        YAP_UserCPredicate("add_edge", c_add_edge, 5);
        YAP_UserCPredicate("make_sparse_vector", c_make_sparse_vector, 6);
        YAP_UserCPredicate("write_sparse_vector", c_write_sparse_vector, 1);
        YAP_UserCPredicate("add_feature_to_sparse_vector", c_add_feature_to_sparse_vector, 3);
        YAP_UserCPredicate("set_target_label", c_set_target_label, 2);
        YAP_UserCPredicate("set_label_name", c_set_label_name, 2);
        YAP_UserCPredicate("set_rejected", c_set_rejected, 1);
        YAP_UserCPredicate("clear_rejected", c_clear_rejected, 0);
        YAP_UserCPredicate("remap_indices", c_remap_indices, 0);
        YAP_UserCPredicate("print_graph_ids", c_print_graph_ids, 0);

        // Feature generator related predicates
        YAP_UserCPredicate("new_feature_generator", c_new_feature_generator, 2);
        YAP_UserCPredicate("delete_feature_generator", c_delete_feature_generator, 1);

        // Model related predicates
        YAP_UserCPredicate("new_model", c_new_model, 2);
        YAP_UserCPredicate("delete_model", c_delete_model, 1);
        YAP_UserCPredicate("model_type", c_model_type, 2);
        YAP_UserCPredicate("write_model", c_write_model, 1);
        YAP_UserCPredicate("load_model", c_load_model, 2);
        YAP_UserCPredicate("save_model", c_save_model, 2);
        YAP_UserCPredicate("check_model_ability", c_check_model_ability, 2);
        YAP_UserCPredicate("train_model", c_train_model, 2);
        YAP_UserCPredicate("test_dataset", c_test_dataset, 3);
        YAP_UserCPredicate("dot_product", c_dot_product, 3);
        YAP_UserCPredicate("set_model_wd", c_set_model_wd, 2);
        YAP_UserCPredicate("reset_reporter", c_reset_reporter, 2);
        YAP_UserCPredicate("reset_reporter", c_reset_reporter3, 3);
        YAP_UserCPredicate("report", c_report, 2);
        YAP_UserCPredicate("report", c_report3, 3);
        YAP_UserCPredicate("save_predictions", c_save_predictions, 3);
        YAP_UserCPredicate("get_prediction", c_get_prediction, 3);
    }
}
