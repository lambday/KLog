#include <iostream>
#include <map>
#include <stdexcept>
#include "yap.h"
#include "graph.h"

std::map<std::string, int> my_map;

static int c_foo(void) {
    YAP_Term arg = YAP_ARG1;
    Term* t = factory(arg);
    std::cout << t->str() << std::endl;
    return 1;
}

static int c_add_vertex(void) {
    YAP_Term arg = YAP_ARG1;
    if (!YAP_IsApplTerm(arg)) {
        std::cerr << "vertex/3 expected" << std::endl;
        return 0;
    }
    try {
        ApplTerm* t = new ApplTerm(arg);
        if (t->get_functor_str() != "vertex" || t->get_arity() != 3) {
            std::cerr << "vertex/3 expected" << std::endl;
            return 0;
        }
        std::cout << t->str() << std::endl;
        IntTerm* id_term = (IntTerm*) (*t)[0];
        int id = (*id_term)();
        //ApplTerm* label_term = (ApplTerm*)(*t)[1];
        Term* label_term = (*t)[1];
        AtomTerm* kind_term = (AtomTerm*) (*t)[2];
        std::cout << "ID= " << id << std::endl;
        std::cout << "label= " << label_term->str() << std::endl;
        // std::cout << "functor= " << label_term->get_functor_str() << std::endl;
        // for (int i=0; i<label_term->get_arity(); ++i) {
        //   Term* argi = (*label_term)[i];
        //   std::cout << "arg[" << i << "]= " << argi->str() << std::endl;
        // }
        std::cout << "kind= " << kind_term->str() << std::endl;
        my_map[label_term->str()]++;
    } catch (std::exception & e) {
        std::cerr << "Exception (C++): " << e.what() << std::endl;
        //    std::cerr << "Type: " << typeid( e ).name( ) << std::endl;
        return 0;
    }
    return 1;
}

static int c_list_vertices(void) {
    for (std::map<std::string, int>::iterator i = my_map.begin(); i != my_map.end(); ++i) {
        std::cout << i->first << "  --->  " << i->second << std::endl;
    }
    return 1;
}

extern "C" { // avoid C++ name mangling

    void init_my_predicates() {
        YAP_UserCPredicate("cfoo", c_foo, 1);
        YAP_UserCPredicate("av", c_add_vertex, 1);
        YAP_UserCPredicate("lv", c_list_vertices, 0);
    }

}
