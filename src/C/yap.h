/// -*- c++ -*-
/// @file   yap.h
/// @author Paolo Frasconi p-f@dsi.unifi.it
/// @brief  Helpers for Yap-C interface
/// 

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

#ifndef yap_h
#define yap_h 1

#include <Yap/YapInterface.h>

#include <vector>
#include <stdexcept>
#include <iterator>
#include <sstream>
#include <cstdio>

class Term 
{
private:
  YAP_Term term;
public:
  Term(YAP_Term t) {
    term = t;
  }
  virtual ~Term() {};
  virtual std::string str(void) const = 0;
  virtual std::string t_str(void) const = 0;
};

Term* factory(YAP_Term t);

class IntTerm : public Term
{
private:
  int v;
public:
  IntTerm(YAP_Term t):Term(t) { 
    v = YAP_IntOfTerm(t); 
  }
  int operator()(void) const {return v;}
  int get_int(void) const {return v;}
  std::string str(void) const {
    char s[1024];
    sprintf(s, "%d", v);
    std::string st(s);
    return st;
  }
  std::string t_str(void) const { return "Int"; }
};

class FloatTerm : public Term
{
private:
  double v;
public:
  FloatTerm(YAP_Term t):Term(t) {
    v = YAP_FloatOfTerm(t); 
  }
  FloatTerm(YAP_Term t, bool castme):Term(t) {
    int iv = YAP_IntOfTerm(t);
    v = double(iv);
  }
  double operator()(void) const {return v;}
  double get_float(void) const {return v;}
  std::string str(void) const {
    char s[1024];
    sprintf(s, "%f", v);
    std::string st(s);
    return st;
  }
  std::string t_str(void) const { return "Float"; }
};

class AtomTerm : public Term
{
private:
  YAP_Atom v;
public:
  AtomTerm(YAP_Term t):Term(t) { 
    v = YAP_AtomOfTerm(t); 
  }
  AtomTerm(YAP_Atom atom):Term(YAP_MkAtomTerm(atom)) { v = atom; }
  YAP_Atom operator()(void) const {return v;}
  std::string str(void) const { return YAP_AtomName(v);}
  std::string t_str(void) const { return "Atom"; }
};

class PairTerm : public Term
{
private:
  Term* head;
  PairTerm* tail;
public:
  PairTerm(YAP_Term t):Term(t) {
    // std::cout << "t= " << t << std::endl;
    head = factory(YAP_HeadOfTerm(t));
    YAP_Term tail_of_term = YAP_TailOfTerm(t);
    // std::cout << "tail_of_term= " << tail_of_term << std::endl;
    // std::cout << "YAP_TermNil()= " << YAP_TermNil() << std::endl;
    if (tail_of_term != YAP_TermNil()) 
      tail = new PairTerm(tail_of_term);
    else
      tail = NULL;
  }
  ~PairTerm() {
    delete tail;
    delete head;
  }
  std::string str(void) const { return (tail ? (".(" + head->str() + "," + tail->str() + ")") : head->str() );}
  std::string t_str(void) const { return "Pair"; }
};


class PrologList : public Term
{
private:
  std::vector<YAP_Term> lst;
public:
  PrologList(YAP_Term t):Term(t) {
    while (t != YAP_TermNil()) {
      lst.push_back(YAP_HeadOfTerm(t));
      t = YAP_TailOfTerm(t);
    }
  }
  int size(void) const {return lst.size();}
  YAP_Term operator[](int i) const {
    if (i>=size()) throw std::out_of_range("PrologList::operator[]");
    return lst[i];
  }
  std::string str(void) const {
    std::string s="[";
    for (int i=0; i<size(); ) {
      s = s + factory(lst[i])->str();
      if (++i < size())
	s = s + ",";
    }
    s = s + "]";
    return s;
  }
  std::string t_str(void) const { return "PrologList"; }
};



class PrologNumericalList : public Term
{
private:
  std::vector<double> lst;
public:
  PrologNumericalList(YAP_Term t):Term(t) {
    while (t != YAP_TermNil()) {
      YAP_Term element = YAP_HeadOfTerm(t);
      if (YAP_IsFloatTerm(element)) {
	lst.push_back(YAP_FloatOfTerm(element));
      }
      if (YAP_IsIntTerm(element)) {
	lst.push_back(double(YAP_IntOfTerm(element)));
      }
      t = YAP_TailOfTerm(t);
    }
  }
  const std::vector<double>& get_list(void) const { return lst; }
  std::string str(void) const {
    std::ostringstream os;
    os << "[";
    for (unsigned i=0; i<lst.size(); ) {
      os << lst[i];
      if (++i < lst.size())
	os << ",";
    }
    os << "]";
    return os.str();
  }
  std::string t_str(void) const { return "PrologNumericalList"; }
};

class PrologAtomList : public Term
{
private:
  std::vector<std::string> lst;
public:
  PrologAtomList(YAP_Term t):Term(t) {
    while (t != YAP_TermNil()) {
      YAP_Term element = YAP_HeadOfTerm(t);
      if (YAP_IsAtomTerm(element)) {
	lst.push_back(YAP_AtomName(YAP_AtomOfTerm(element)));
      }
      t = YAP_TailOfTerm(t);
    }
  }
  const std::vector<std::string>& get_list(void) { return lst; }
  std::string str(void) const {
    std::ostringstream os;
    os << "[";
    for (unsigned i=0; i<lst.size(); ) {
      os << lst[i];
      if (++i < lst.size())
	os << ",";
    }
    os << "]";
    return os.str();
  }
  std::string t_str(void) const { return "PrologAtomList"; }
};


class ApplTerm : public Term
{
private:
  YAP_Functor functor;
  AtomTerm functor_name;
  int arity;
  std::vector<Term*> args;
public:
  ApplTerm(YAP_Term t):Term(t), functor(YAP_FunctorOfTerm(t)), functor_name(YAP_NameOfFunctor(functor)) { 
    //std::cout << "*** ApplTerm constructor" << std::endl;
    arity = YAP_ArityOfFunctor(functor);
    //std::cout << "Arity= " << arity << std::endl;
    for (int i=0; i<arity; ++i) {
      YAP_Term ith_arg = YAP_ArgsOfTerm(t)[i];
      args.push_back(factory(ith_arg));
    }
  }
  ~ApplTerm() {
    for (int i=0; i<arity; ++i) {
      delete args[i];
    }
  }
  int get_arity(void) const {return arity;}
  AtomTerm get_functor_name(void) const { return functor_name; }
  std::string get_functor_str(void) const { return YAP_AtomName(YAP_NameOfFunctor(functor)); }
  Term* operator[](int i) const {
    if (i>=arity) throw std::out_of_range("ApplTerm::operator[]");
    return args[i];
  }
  std::string str(void) const {
    std::string s = get_functor_str();
    s = s + "(";
    for (int i=0; i<arity; ++i) {
      s = s + args[i]->str();
      if (i==arity-1)
	s = s + ")";
      else
	s = s + ",";
    }
    return s;
  }
  std::string t_str(void) const { return "Appl"; }
};

class UnknownTerm : public Term
{
public:
  UnknownTerm(YAP_Term t):Term(t) { 
  }
  std::string str(void) const { return ""; }
  std::string t_str(void) const { return "Unknown"; }
};

// ***************************** Creators ********************************

std::string make_message(std::string s, YAP_Term t) {
  Term* tt = factory(t);
  std::string message = s + " (" + tt->t_str() + "): " + tt->str();
  delete tt;
  return message;
}

class TermCreator
{
public:
  virtual Term* create_term(YAP_Term t) = 0;
  virtual ~TermCreator() {};
};

class IntTermCreator : public TermCreator
{
public:
  IntTerm* create_term(YAP_Term t) {
    if (!YAP_IsIntTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- int expected, got", t)));
    return new IntTerm(t);
  }
};

class FloatTermCreator : public TermCreator
{
public:
  FloatTerm* create_term(YAP_Term t) {
    if (YAP_IsFloatTerm(t))
      return new FloatTerm(t);
    else if (YAP_IsIntTerm(t))
      return new FloatTerm(t,true); // simple int->float cast
    else
      throw(std::logic_error(make_message("TYPE ERROR- float expected, got", t)));
    // if (!YAP_IsFloatTerm(t))
    //   throw(std::logic_error(make_message("TYPE ERROR- float expected, got", t)));
    // return new FloatTerm(t);
  }
};

class AtomTermCreator : public TermCreator
{
public:
  AtomTerm* create_term(YAP_Term t) {
    if (!YAP_IsAtomTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- atom expected, got", t)));
    return new AtomTerm(t);
  }
};

class AtomicTermCreator : public TermCreator
{
public:
  Term* create_term(YAP_Term t) {
    if (YAP_IsAtomTerm(t)) {
      return new AtomTerm(t);
    } else if (YAP_IsIntTerm(t)) {
      return new IntTerm(t);
    } else if (YAP_IsFloatTerm(t)) {
      return new FloatTerm(t);
    } else
      throw(std::logic_error(make_message("TYPE ERROR- atomic type expected, got", t)));
  } 
};

class PairTermCreator : public TermCreator
{
public:
  PairTerm* create_term(YAP_Term t) {
    if (!YAP_IsPairTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- pair expected, got", t)));
    return new PairTerm(t);
  }
};

class PrologListCreator : public TermCreator
{
public:
  // ok if (Pair || (Atom && Nil))
  PrologList* create_term(YAP_Term t) {
    if ( !(YAP_IsPairTerm(t) || (YAP_IsAtomTerm(t) && t==YAP_TermNil())) )
    // if (!YAP_IsPairTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- list expected, got", t)));
    return new PrologList(t);
  }
};

class PrologNumericalListCreator : public TermCreator
{
public:
  PrologNumericalList* create_term(YAP_Term t) {
    if ( !(YAP_IsPairTerm(t) || (YAP_IsAtomTerm(t) && t==YAP_TermNil())) )
    // if (!YAP_IsPairTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- list expected, got", t)));
    return new PrologNumericalList(t);
  }
};

class PrologAtomListCreator : public TermCreator
{
public:
  PrologAtomList* create_term(YAP_Term t) {
    if ( !(YAP_IsPairTerm(t) || (YAP_IsAtomTerm(t) && t==YAP_TermNil())) )
    // if (!YAP_IsPairTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- list expected, got", t)));
    return new PrologAtomList(t);
  }
};

class ApplTermCreator : public TermCreator
{
public:
  ApplTerm* create_term(YAP_Term t) {
    if (!YAP_IsApplTerm(t))
      throw(std::logic_error(make_message("TYPE ERROR- compound term expected, got", t)));
    return new ApplTerm(t);
  }
};

Term* factory(YAP_Term t) {
  Term* term;
  if (YAP_IsIntTerm(t)) { term = new IntTerm(t); }
  else if (YAP_IsFloatTerm(t)) { term = new FloatTerm(t);}
  else if (YAP_IsAtomTerm(t)) { term = new AtomTerm(t);}
  else if (YAP_IsPairTerm(t)) { term = new PairTerm(t);}
  else if (YAP_IsApplTerm(t)) { term = new ApplTerm(t);}
  else { term = new UnknownTerm(t);}
  return term;
}

#endif
