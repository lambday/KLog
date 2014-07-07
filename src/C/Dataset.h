/// -*- c++ -*-
/// @file   Dataset.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Singleton for storing graphs and feature vectors

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



#ifndef DATASET_H_
#define DATASET_H_

#include <vector>
#include <map>
#include <set>
#include <tr1/unordered_map>
#include "GraphClass.h"
#include "CaseWrapper.h"
#include "libsvm/svm.h"
#include "FeatureGeneratorPool.h"
#include "kLogMaster.h"
//*** Replacing set -> vector for cases in one interpretation.  Set
// was not useful because only one insert and no search is ever done
// (only scan). Additionally, for sequence labeling it generates cases
// in the same order they appear in the data file, which is necessary
// for external learners such as svm_hmm or CFR.

//typedef __gnu_cxx::hash_map<std::string,TempGraph> DataSet;
typedef std::map<std::string, GraphClass*> GDataset;
// typedef std::map<std::string,std::set<std::string> > Cases_T;
//***
typedef std::map<std::string, std::vector<std::string> > Cases_T;
//***
typedef std::map<double, std::string> SymLabelMap_T;
typedef std::map<std::string, double> NumLabelMap_T;




// -------------------------------------

class Dataset {
private:
    GDataset g_dataset; // for each interpretation, a graph pointer
    Cases_T cases; // for each interpretation, a set of case identifiers
    typedef std::map<std::string, CaseWrapper*> SVDataset;
    typedef std::map<std::string, double> Label_T;
    SVDataset sv_dataset;
    Label_T label;
    SymLabelMap_T sym_label;
    NumLabelMap_T num_label;
    // ***
    typedef std::set<std::string> Rejected_T;
    Rejected_T rejected;
    // ***

    Dataset() {
        remapped_indices = false;
    };
    Dataset(Dataset const& rhs);
    Dataset& operator=(Dataset const& rhs);


    bool remapped_indices;

    /** This is necessary for external software that needs sparse
        vectors with bounded feature numbers (like svm-light).  After
        indices have been remapped, new feature vectors cannot be added
        by the kLog feature generators since "new" feature indices might
        appear and "old" feature indices are lost. Hence an exception
        will be thrown if make_sparse_vector is called after indices
        have been remapped.
     */

public:

    static Dataset& get_instance() {
        static Dataset instance;
        return instance;
    }

    GraphClass* get_graph(const std::string& gid) {
        GDataset::iterator found = g_dataset.find(gid);
        if (found == g_dataset.end()) {
            KLOG_THROW("graph-id >" << gid << "< not found");
        }
        return (*found).second;
    }

    CaseWrapper* get_sparse_vector(const std::string& case_id) {
        SVDataset::iterator found = sv_dataset.find(case_id);
        if (found == sv_dataset.end()) {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
        return (*found).second;
    }

    void add_feature_to_sparse_vector(const std::string& case_id, const std::string& feature_str, double feature_val) {
        SVDataset::iterator found = sv_dataset.find(case_id);
        if (found == sv_dataset.end()) {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
        CaseWrapper* cw = (*found).second;
        unsigned h = HashFunc(feature_str);
        cw->set(h, feature_val);
    }

    double get_label(const std::string& case_id) {
        Label_T::iterator found = label.find(case_id);
        if (found == label.end()) {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
        return (*found).second;
    }
    // ***

    bool get_rejected(const std::string& case_id) {
        SVDataset::iterator found = sv_dataset.find(case_id);
        if (found == sv_dataset.end()) {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
        return (rejected.find(case_id) != rejected.end());
    }

    void clear_rejected(void) {
        rejected.clear();
    }

    void remap_indices(void) {
        std::tr1::unordered_map<int, int> remap;
        int n_features = 0;
        for (SVDataset::iterator i = sv_dataset.begin(); i != sv_dataset.end(); ++i) {
            std::ostringstream os;
            os << *(i->second);
            std::string data = os.str();
            std::vector<std::string> items = split(data);
            CaseWrapper* phi = (i->second)->make_empty_object();
            for (unsigned j = 0; j < items.size(); ++j) {
                size_t index = items[j].find(':', 0);
                int feature = atoi(items[j].substr(0, index).c_str());
                double val = atof(items[j].substr(index + 1).c_str());
                std::tr1::unordered_map<int, int>::iterator found = remap.find(feature);
                if (found == remap.end()) {
                    n_features += 1;
                    remap[feature] = n_features;
                    phi->set(n_features, val);
                } else
                    phi->set(remap[feature], val);
            }
            delete i->second;
            i->second = phi;
        }
        remapped_indices = true;
    }
    // ***

    Cases_T& get_cases(void) {
        return cases;
    }

    void add_graph_if_not_exists(const std::string& gid) {
        GDataset::iterator found = g_dataset.find(gid);
        if (found == g_dataset.end()) {
            GraphClass* g = new GraphClass(gid);
            g_dataset[gid] = g;
        }
    }

    void add_graph(const std::string& gid) {
        GDataset::iterator found = g_dataset.find(gid);
        if (found != g_dataset.end()) {
            KLOG_THROW("Duplicate graph identifier >" << gid << "<");
        }
        GraphClass* g = new GraphClass(gid);
        g_dataset[gid] = g;
    }

    void delete_graph(const std::string& gid) {
        GDataset::iterator found = g_dataset.find(gid);
        if (found != g_dataset.end()) {
            delete (*found).second;
            g_dataset.erase(found);
        } else {
            KLOG_THROW("Invalid graph identifier >" << gid << "<");
        }
    }

    void write_graph(const std::string& gid) {
        GDataset::iterator found = g_dataset.find(gid);
        if (found != g_dataset.end()) {
            (*found).second->Output(std::cout);
        } else {
            KLOG_THROW("Invalid graph identifier >" << gid << "<");
        }
    }

    void export_graph(const std::string& gid, const std::string& filename, const std::string& format) {
        GDataset::iterator found = g_dataset.find(gid);
        if (found != g_dataset.end()) {
            (*found).second->ExportGraph(filename, format);
        } else {
            KLOG_THROW("Invalid graph identifier >" << gid << "<");
        }
    }

    void save_as_libsvm_file(const std::string& filename) {
        std::ofstream libsvm_stream;
        libsvm_stream.open(filename.c_str(), ios::out);
        for (SVDataset::iterator i = sv_dataset.begin(); i != sv_dataset.end(); ++i) {
            // SVector* svc = i->second;
            // SVector* sv = i->second;
            // std::cout << *svc << std::endl;
            // std::cout << *sv << std::endl;
            Label_T::iterator found = label.find(i->first);
            if (found != label.end()) {
                libsvm_stream << found->second << " " << *(i->second) << " # " << i->first;
                if (sym_label[found->second] != "")
                    libsvm_stream << "(" << sym_label[found->second] << ")" << std::endl;
                else
                    libsvm_stream << "(" << found->second << ")" << std::endl;
            } else {
                KLOG_THROW("Case identifier >" << i->first << "< has no label");
            }
        }
        libsvm_stream.close();
    }

    /** 
     * Save all feature vectors associated with a given set of
     * interpretations. This is mainly useful for external learners. The
     * format is compatible with libsvm and svm_light. Optionally can
     * save in a format compatible with svm_struct (using qid's).
     * 
     * @param basename the basename where all files are saved, including path
     * @param interpretation_ids vector of interpretation identifiers
     * @param write_qid if true, add a qid field for svm_struct. qid are
     * numbered incrementally from 1 to the # of interpretations and
     * there is one qid for each interpretation identifier.
     */
    void save_as_libsvm_file(const std::string& basename,
            const std::vector<std::string>& interpretation_ids,
            bool write_qid = false) {
        std::ofstream libsvm_stream;
        libsvm_stream.open((basename + ".sv").c_str(), ios::out);
        std::ofstream cid_stream;
        cid_stream.open((basename + ".cid").c_str(), ios::out);
        check_sorted_ascending(interpretation_ids); // or the trick below does not work
        std::vector<std::string>::const_iterator ii = interpretation_ids.begin();
        Cases_T::const_iterator di = get_cases().begin();
        int qid = 0;
        while (ii != interpretation_ids.end() && di != get_cases().end()) {
            qid += 1;
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
                for (std::vector<std::string>::const_iterator i = c.begin(); i != c.end(); ++i) {
                    //***
                    if (get_rejected(*i))
                        continue;
                    libsvm_stream << get_label(*i);
                    if (write_qid)
                        libsvm_stream << " qid:" << qid;
                    libsvm_stream << " " << *get_sparse_vector(*i) << std::endl;
                    cid_stream << *i;
                    if (sym_label[get_label(*i)] != "")
                        cid_stream << " " << sym_label[get_label(*i)];
                    else
                        cid_stream << " no_sym_label";
                    cid_stream << " " << get_label(*i) << std::endl;
                }
                ++di;
                ++ii;
            }
        }
        libsvm_stream.close();
        cid_stream.close();
    }

    void cleanup_vectors(void) {
        std::cout << "***************** WARNING: Cleaning up vectors!" << std::endl;
        for (SVDataset::const_iterator i = sv_dataset.begin(); i != sv_dataset.end(); ++i) {
            delete i->second;
        }
        sv_dataset.clear();
        cases.clear();
        rejected.clear();
        sym_label.clear();
        num_label.clear();
    }

    void cleanup_data(void) {
        cleanup_vectors();
        for (GDataset::const_iterator i = g_dataset.begin(); i != g_dataset.end(); ++i) {
            delete i->second;
        }
        g_dataset.clear();
        label.clear();
    }
    // bool g_has_a_dead(const GraphClass* const g) {
    //   for (unsigned i=0; i<g->VertexSize(); ++i) {
    //     if (g->GetVertexDead(i))
    //         return true;
    //   }
    //   return false;
    // }

    void make_sparse_vector(const std::string& mtype,
            const std::string& fgid,
            const std::string& gid,
            const std::string& slice_id,
            const std::string& case_id, const std::vector<double>& viewpoints) {
        // std::cout << "Make sparse vector graph= " << gid 
        //           << " focusing on: [";
        // for (unsigned i=0; i<viewpoints.size(); ++i)
        //   std::cout << int(viewpoints[i]) << " ";
        // std::cout << "] case id: " << case_id
        //           << std::endl;

        // static int cnt=0;
        SVDataset::const_iterator i = sv_dataset.find(case_id);
        if (i == sv_dataset.end()) {
            if (remapped_indices)
                KLOG_THROW("Cannot add - Indices in the data set have been remapped."
                    << std::endl
                    << "If using external models, please ensure that all feature vectors"
                    << std::endl
                    << "are created before entering a k-fold-cv or similar"
                    << std::endl
                    << "using Prolog predicate generate_all/3"
                    );
            GDataset::const_iterator found = g_dataset.find(gid);
            if (found == g_dataset.end())
                KLOG_THROW("Invalid graph identifier >" << gid << "<");
            const GraphClass * const g = g_dataset[gid];
            // char sssname[256];
            // sprintf(sssname,"%s-%3d-%d",gid.c_str(),cnt++,g);
            // if (!g_has_a_dead(g))
            //   throw std::out_of_range("No dead vertices in >" + gid + "< in Dataset::make_sparse_vector()");
            // g->ExportGraph(sssname,"dot");
            std::vector<unsigned> vp(viewpoints.size());
            for (unsigned v = 0; v < viewpoints.size(); ++v) {
                vp[v] = unsigned(viewpoints[v]);
            }
            // std::cout << "C++ make_sparse_vector"  << mtype 
            //           << " " << fgid
            //           << " " << gid
            //           << " " << case_id;
            // for (unsigned v=0; v<viewpoints.size(); ++v)
            //   std::cout << " " << viewpoints[v];
            // std::cout << " " << std::endl;

            //      g->ComputePairwiseDistanceInformation();
            FeatureGenerator* f = The_FeatureGeneratorPool.get_feature_generator(fgid);
            // SVector* x = new SVector();
            CaseWrapper* x = new_wrapper(mtype);
            if (The_Master.get_save_subgraphs_directory() != "") {
                std::string cmd = "mkdir -p " + The_Master.get_save_subgraphs_directory();
                system(cmd.c_str());
                string s = case_id;
                std::replace(s.begin(), s.end(), '/', '-');
                g->ComputePairwiseDistanceInformation(atoi(f->get_param("distance").c_str()), atoi(f->get_param("radius").c_str()));
                // std::cout << g->Serialize() << std::endl;
                std::cout << "Saving into " << The_Master.get_save_subgraphs_directory() + "/" + s << std::endl;
                g->SaveAsMatlabFile(The_Master.get_save_subgraphs_directory() + "/" + s, vp);
            }
            f->generate_feature_vector(*g, x, vp);
            sv_dataset[case_id] = x;
            // std::cout << *(sv_dataset[case_id]) << std::endl;
            // cases[slice_id].insert(case_id);
            //***
            cases[slice_id].push_back(case_id);
            //***
            //pmem();
        } // else the sparse vector for this case has been computed already, do nothing
    }

    void write_sparse_vector(const std::string& case_id) {
        SVDataset::iterator found = sv_dataset.find(case_id);
        if (found != sv_dataset.end()) {
            std::cout << *(sv_dataset[case_id]) << std::endl;
        } else {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
    }

    double dot_product(const std::string& case_id1, const std::string& case_id2) {
        SVDataset::iterator found1 = sv_dataset.find(case_id1);
        SVDataset::iterator found2 = sv_dataset.find(case_id2);
        CaseWrapper* v1;
        CaseWrapper* v2;
        if (found1 != sv_dataset.end()) {
            v1 = sv_dataset[case_id1];
        } else {
            KLOG_THROW("Invalid case identifier >" << case_id1 << "<");
        }
        if (found2 != sv_dataset.end()) {
            v2 = sv_dataset[case_id2];
        } else {
            KLOG_THROW("Invalid case identifier >" << case_id2 << "<");
        }
        double value = v1->dot_product(v2);
        // std::cout << "Dot between " 
        //           << *v1 << " and " << *v2 << " = "
        //           << value << std::endl;
        return value;
    }

    void set_target_label(const std::string& case_id, const double target_label) {
        SVDataset::iterator found = sv_dataset.find(case_id);
        if (found != sv_dataset.end()) {
            label[case_id] = target_label;
        } else {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
        // std::cout << "target label for case id "
        //           << case_id << " set to " << target_label << std::endl;
    }
    // ***

    void set_rejected(const std::string& case_id) {
        SVDataset::iterator found = sv_dataset.find(case_id);
        if (found != sv_dataset.end()) {
            rejected.insert(case_id);
        } else {
            KLOG_THROW("Invalid case identifier >" << case_id << "<");
        }
    }
    // ***

    void set_label_name(const std::string& label, const double numeric_label) {
        sym_label[numeric_label] = label;
        num_label[label] = numeric_label;
    }

    const std::map<double, std::string>& get_sym_label(void) {
        return sym_label;
    }

    const std::map<std::string, double>& get_num_label(void) {
        return num_label;
    }

    void print_graph_ids(void) {
        for (GDataset::iterator i = g_dataset.begin(); i != g_dataset.end(); ++i)
            std::cout << i->first << std::endl;
    }

    void dump_case_map() { // debugging
        for (Cases_T::const_iterator c = cases.begin(); c != cases.end(); ++c) {
            std::cout << "Case set for " << c->first << ":";
            //***
            // std::set<std::string> s = c->second;
            std::vector<std::string> s = c->second;
            //***
            // for (std::set<std::string>::const_iterator si = s.begin(); si!=s.end(); ++si) {
            //***
            for (std::vector<std::string>::const_iterator si = s.begin(); si != s.end(); ++si) {
                //***
                std::cout << " " << *si;
            }
            std::cout << std::endl;
        }
    }

};

extern Dataset& The_Dataset;







#endif /* DATASET_H_ */
