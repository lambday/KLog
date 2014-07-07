/// -*- c++ -*-
/// @file   FlagsService.h
/// @author Paolo Frasconi <p-f@dsi.unifi.it>
/// @brief  Add flags (options) to c++ objects

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


#ifndef FLAGSSERVICE_H_
#define FLAGSSERVICE_H_

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <math.h>
#include <string>
#include <stdexcept>
#include <map>
#include <sstream>
#include <algorithm>
#include <iostream>
#include <typeinfo>
#include <vector>

#include "Utility.h"

/** 
 * @class SetFlagHook 
 * 
 * @brief Base class for a simple callback mechanism to be invoked
 * when a flag is changed.
 */
class SetFlagHook {
public:
    virtual void call(void) = 0;
};

/** 
 * @class FlagsTraits
 * 
 * @brief Contains traits of flags. The name of a flag is used by
 * Prolog's predicates set_klog_flag and get_klog_flag. The
 * description is for the interactive help.
 */
class FlagsTraits {
private:
    std::string name;
    std::string description;
    SetFlagHook* setflaghook;
public:

    FlagsTraits(const std::string& name, const std::string& description, SetFlagHook* setflaghook = NULL) {
        this->name = name;
        this->description = description;
        this->setflaghook = setflaghook;
    }

    std::string& get_name(void) {
        return name;
    }

    std::string& get_description(void) {
        return description;
    }

    SetFlagHook* get_setflaghook(void) {
        return setflaghook;
    }

    virtual void dummy(void) {
    }; // make dynamic_cast possible...
};

class BoolFlagsTraits : public FlagsTraits {
private:
    bool* ref;
public:

    BoolFlagsTraits(bool* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook) :
    FlagsTraits(name, description, setflaghook) {
        this->ref = ref;
    }

    bool* get_ref(void) {
        return ref;
    }
};

class IntFlagsTraits : public FlagsTraits {
private:
    int* ref;
public:

    IntFlagsTraits(int* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook) :
    FlagsTraits(name, description, setflaghook) {
        this->ref = ref;
    }

    int* get_ref(void) {
        return ref;
    }
};

class UnsignedFlagsTraits : public FlagsTraits {
private:
    unsigned* ref;
public:

    UnsignedFlagsTraits(unsigned* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook) :
    FlagsTraits(name, description, setflaghook) {
        this->ref = ref;
    }

    unsigned* get_ref(void) {
        return ref;
    }
};

class DoubleFlagsTraits : public FlagsTraits {
private:
    double* ref;
public:

    DoubleFlagsTraits(double* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook) :
    FlagsTraits(name, description, setflaghook) {
        this->ref = ref;
    }

    double* get_ref(void) {
        return ref;
    }
};

class StringFlagsTraits : public FlagsTraits {
private:
    std::string* ref;
public:

    StringFlagsTraits(std::string* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook) :
    FlagsTraits(name, description, setflaghook) {
        this->ref = ref;
    }

    std::string* get_ref(void) {
        return ref;
    }
};

/**
 * @class FlagsServiceClient
 *
 * Interface for clients of the Flags service. Every class that needs
 * to use the Flags service should inherit from this. A flag is a
 * prolog reference to a data member of the class using the
 * service. The class is expected to push its own flags using the
 * method new_flag. Currently only bool, int, unsigned, double, and
 * std::string are valid types for data members accessible via
 * flags. In their constructors, derived classes should initialize the
 * base class with a string (id) that is used to identify the specific
 * object of that class. Then Prolog can communicate with the class
 * via the predicates set_klog_flag(+Id,+FlagName,+FlagValue) and
 * get_klog_flag(+Id,+FlagName,-FlagValue). Prolog values are cast
 * into the right type of the object variable automatically. The
 * derived class is responsible for raising exceptions if the values
 * are not as expected. Exceptions for invalid flag names are raised
 * here.
 */
class FlagsServiceClient {
private:
    std::string id_for_flags_service;
    typedef std::map<std::string, FlagsTraits*> FMap;
    FMap flags_traits;
public:

    std::string id_str(void) const {
        return id_for_flags_service;
    }

    void new_flag(bool* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook = NULL) {
        flags_traits[name] = new BoolFlagsTraits(ref, name, description, setflaghook);
    }

    void new_flag(int* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook = NULL) {
        flags_traits[name] = new IntFlagsTraits(ref, name, description, setflaghook);
    }

    void new_flag(unsigned* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook = NULL) {
        flags_traits[name] = new UnsignedFlagsTraits(ref, name, description, setflaghook);
    }

    void new_flag(double* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook = NULL) {
        flags_traits[name] = new DoubleFlagsTraits(ref, name, description, setflaghook);
    }

    void new_flag(std::string* ref, const std::string& name, const std::string& description, SetFlagHook* setflaghook = NULL) {
        flags_traits[name] = new StringFlagsTraits(ref, name, description, setflaghook);
    }

    std::string document_flags(void) const {
        std::ostringstream outs;
        outs.setf(std::ios::left, std::ios::adjustfield);
        for (FMap::const_iterator i = flags_traits.begin(); i != flags_traits.end(); ++i) {
            outs << std::setw(23) << id_for_flags_service + "::" + i->second->get_name() << " [";
            BoolFlagsTraits* pb;
            IntFlagsTraits* pi;
            UnsignedFlagsTraits* pu;
            DoubleFlagsTraits* pd;
            StringFlagsTraits* ps;
            if ((pb = dynamic_cast<BoolFlagsTraits*> (i->second)) != NULL)
                outs << *(pb->get_ref());
            else if ((pi = dynamic_cast<IntFlagsTraits*> (i->second)) != NULL)
                outs << *(pi->get_ref());
            else if ((pu = dynamic_cast<UnsignedFlagsTraits*> (i->second)) != NULL)
                outs << *(pu->get_ref());
            else if ((pd = dynamic_cast<DoubleFlagsTraits*> (i->second)) != NULL)
                outs << *(pd->get_ref());
            else if ((ps = dynamic_cast<StringFlagsTraits*> (i->second)) != NULL)
                outs << *(ps->get_ref());
            else
                KLOG_THROW("Nasty bug - unrecognized flag type");
            outs << "]" << std::endl << i->second->get_description() << std::endl << std::endl;
        }
        return outs.str();
    }
    virtual ~FlagsServiceClient();

    void set_flag(const std::string& flag_name, const std::string& flag_value) {
        FMap::iterator found = flags_traits.find(flag_name);
        if (found == flags_traits.end())
            KLOG_THROW("Unknown flag name >" << flag_name << "<");
        BoolFlagsTraits* pb;
        IntFlagsTraits* pi;
        UnsignedFlagsTraits* pu;
        DoubleFlagsTraits* pd;
        StringFlagsTraits* ps;
        if ((pb = dynamic_cast<BoolFlagsTraits*> (found->second)) != NULL) {
            // std::cerr << "setting " << flag_name << " of type bool to value " << flag_value << std::endl;
            *(pb->get_ref()) = (flag_value == "true" || flag_value == "on" || flag_value == "yes");
        } else if ((pi = dynamic_cast<IntFlagsTraits*> (found->second)) != NULL) {
            // std::cerr << "setting " << flag_name << " of type int to value " << flag_value << std::endl;
            *(pi->get_ref()) = atoi(flag_value.c_str());
        } else if ((pu = dynamic_cast<UnsignedFlagsTraits*> (found->second)) != NULL) {
            // std::cerr << "setting " << flag_name << " of type unsigned to value " << flag_value << std::endl;
            *(pu->get_ref()) = unsigned(atoi(flag_value.c_str()));
        } else if ((pd = dynamic_cast<DoubleFlagsTraits*> (found->second)) != NULL) {
            // std::cerr << "setting " << flag_name << " of type double to value " << flag_value << std::endl;
            *(pd->get_ref()) = atof(flag_value.c_str());
        } else if ((ps = dynamic_cast<StringFlagsTraits*> (found->second)) != NULL) {
            // std::cerr << "setting " << flag_name << " of type string to value " << flag_value << std::endl;
            *(ps->get_ref()) = flag_value;
        } else
            KLOG_THROW("Nasty bug - unrecognized flag type");
        if (found->second->get_setflaghook()) { // if non nil, call the set flag hook function after setting the flag
            found->second->get_setflaghook()->call();
        }
    }

    std::string get_flag(const std::string& flag_name) {
        FMap::iterator found = flags_traits.find(flag_name);
        std::stringstream oss;
        if (found == flags_traits.end())
            KLOG_THROW("Unknown flag name >" << flag_name << "<");
        BoolFlagsTraits* pb;
        IntFlagsTraits* pi;
        UnsignedFlagsTraits* pu;
        DoubleFlagsTraits* pd;
        StringFlagsTraits* ps;
        if ((pb = dynamic_cast<BoolFlagsTraits*> (found->second)) != NULL)
            oss << *(pb->get_ref());
        else if ((pi = dynamic_cast<IntFlagsTraits*> (found->second)) != NULL)
            oss << *(pi->get_ref());
        else if ((pu = dynamic_cast<UnsignedFlagsTraits*> (found->second)) != NULL)
            oss << *(pu->get_ref());
        else if ((pd = dynamic_cast<DoubleFlagsTraits*> (found->second)) != NULL)
            oss << *(pd->get_ref());
        else if ((ps = dynamic_cast<StringFlagsTraits*> (found->second)) != NULL)
            oss << *(ps->get_ref());
        else
            KLOG_THROW("Nasty bug - unrecognized flag type");
        return oss.str();
    }
    FlagsServiceClient(const std::string& id_for_flags_service);
};

/**
 * @class FlagsService
 *
 * A singleton storing all the clients of the flags service. Prolog
 * calls for seting, gettin, and documenting flags are intercepted
 * here and dispatched to the right client by looking up the client's
 * id in the internal storage of this object.
 */

class FlagsService {
public:
    typedef std::map<std::string, FlagsServiceClient*> Map;
private:

    Map pool;

    FlagsService() {
    };
    FlagsService(FlagsService const& rhs);
    FlagsService& operator=(FlagsService const& rhs);

    FlagsServiceClient* find_range_checked(const std::string& client_id) const {
        Map::const_iterator found = pool.find(client_id);
        if (found == pool.end())
            KLOG_THROW("Flags client identifier >" << client_id << "< not registered");
        return found->second;
    }

public:

    static FlagsService& get_instance() {
        static FlagsService instance;
        return instance;
    }

    // static unsigned to_unsigned(const std::string& s) { return unsigned(atoi(s.c_str())); }
    // static int to_int(const std::string& s) { return atoi(s.c_str()); }
    // static double to_double(const std::string& s) { return atof(s.c_str()); }
    // static bool to_bool(const std::string& s) { 
    //   std::string r=s;
    //   std::transform(r.begin(), r.end(), r.begin(), ::tolower);
    //   return (r=="true" || r=="yes" || r=="on" || r=="t");
    // }
    // template <typename T>
    // static std::string to_string(const T x) {
    //   std::stringstream oss; oss << x; return oss.str();
    // }

    // Register a new client in the pool. The client must respond to the
    // FlagsClient interface defined above.

    void register_flags_service_client(FlagsServiceClient* c) {
        Map::iterator found = pool.find(c->id_str());
        if (found != pool.end()) {
            KLOG_THROW("Duplicate client identifier >" << c->id_str() << "<");
        }
        pool[c->id_str()] = c;
    }

    void unregister_flags_service_client(FlagsServiceClient* c) {
        Map::iterator found = pool.find(c->id_str());
        if (found == pool.end()) {
            KLOG_THROW("Unknown client identifier >" << c->id_str() << "<");
        }
        pool.erase(c->id_str());
    }

    void set_flag(const std::string client_id, const std::string& flag_name, const std::string& flag_value) {
        find_range_checked(client_id)->set_flag(flag_name, flag_value);
    }

    std::string get_flag(const std::string client_id, const std::string& flag_name) {
        return find_range_checked(client_id)->get_flag(flag_name);
    }

    std::string document_flags(const std::string client_id) {
        return find_range_checked(client_id)->document_flags();
    }

    bool is_registered_client(const std::string& client_id) const {
        Map::const_iterator found = pool.find(client_id);
        return ( found != pool.end());
    }

    void get_client_ids(std::vector<std::string>& ids) const {
        for (Map::const_iterator it = pool.begin(); it != pool.end(); ++it) {
            ids.push_back(it->first);
        }
    }

    void get_client_ids_alt(Map::const_iterator& begin, Map::const_iterator& end) const {
        begin = pool.begin();
        end = pool.end();
    }

};

extern FlagsService& The_FlagsService;



#endif /* FLAGSSERVICE_H_ */
