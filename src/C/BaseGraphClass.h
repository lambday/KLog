/// -*- c++ -*-
/// @file   BaseGraphClass.h
/// @author Fabrizio Costa <costa@informatik.uni-freiburg.de>
/// @brief  Base graph class

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

#ifndef BASEGRAPHCLASS_H
#define BASEGRAPHCLASS_H

#include <set>
#include <string>
using namespace std;

class BaseGraphClass{
  friend ostream& operator<<(ostream& out, const BaseGraphClass& aSG);
  class EdgeClass{
  public:
    EdgeClass();
    EdgeClass(unsigned aDestVertexID,unsigned aEdgeID);
  public:
    unsigned mDestVertexID;
    unsigned mEdgeID;
  };
public:
  BaseGraphClass();
  unsigned GetVertexInducedRootedSubGraph(const set<unsigned>& aVertexSet, unsigned aNominalRootIndex, BaseGraphClass& oG)const;
  vector<unsigned> GetVertexInducedSubGraph(const set<unsigned>& aVertexSet, BaseGraphClass& oG)const;
  unsigned InsertVertex();
  unsigned InsertEdge(unsigned aSrcVertexID, unsigned aDestVertexID);
  void SetVertexNumericAttributeList(unsigned aID,const vector<double>& aAttributeList);
  void SetVertexNumericAttributeList(unsigned aID,unsigned aAttributeID, double aValue);
  void SetVertexSymbolicAttributeList(unsigned aID,const vector<string>& aAttributeList);
  void SetVertexSymbolicAttributeList(unsigned aID,unsigned aAttributeID, const string& aValue);
  void SetVertexStatusAttributeList(unsigned aID,const vector<bool>& aAttributeList);
  void SetVertexStatusAttributeList(unsigned aID,unsigned aAttributeID, bool aValue);
  void SetEdgeNumericAttributeList(unsigned aID,const vector<double>& aAttributeList);
  void SetEdgeNumericAttributeList(unsigned aID,unsigned aAttributeID, double aValue);
  void SetEdgeNumericAttributeList(unsigned aSrcID, unsigned aDestID,unsigned aAttributeID, double aValue);
  void SetEdgeSymbolicAttributeList(unsigned aID,const vector<string>& aAttributeList);
  void SetEdgeSymbolicAttributeList(unsigned aID,unsigned aAttributeID, const string& aValue);
  void SetEdgeSymbolicAttributeList(unsigned aSrcID, unsigned aDestID,unsigned aAttributeID, const string& aValue);
  void SetEdgeStatusAttributeList(unsigned aID,const vector<bool>& aAttributeList);
  void SetEdgeStatusAttributeList(unsigned aID,unsigned aAttributeID, bool aValue);
  void SetEdgeStatusAttributeList(unsigned aSrcID, unsigned aDestID,unsigned aAttributeID, bool aValue);
  void SetVertexSymbolicID(unsigned aID,string aSID);
  string GetVertexSymbolicID(unsigned aID) const;
  vector<string> GetVertexSymbolicAttributeList(unsigned aID)const;
  const vector<string>& GetVertexSymbolicAttributeListRef(unsigned aID)const;
  vector<double> GetVertexNumericAttributeList(unsigned aID)const;
  vector<bool> GetVertexStatusAttributeList(unsigned aID)const;
  string GetVertexSymbolicAttributeList(unsigned aID, unsigned aAttributeID)const;
  double GetVertexNumericAttributeList(unsigned aID, unsigned aAttributeID)const;
  bool GetVertexStatusAttributeList(unsigned aID, unsigned aAttributeID)const;
  vector<string> GetEdgeSymbolicAttributeList(unsigned aSrcID, unsigned aDestID)const;
  string GetEdgeSymbolicAttributeList(unsigned aSrcID, unsigned aDestID, unsigned aAttributeID)const;
  vector<string> GetEdgeSymbolicAttributeList(unsigned aEdgeID)const;
  vector<double> GetEdgeNumericAttributeList(unsigned aSrcID, unsigned aDestID)const;
  double GetEdgeNumericAttributeList(unsigned aSrcID, unsigned aDestID, unsigned aAttributeID)const;
  vector<bool> GetEdgeStatusAttributeList(unsigned aSrcID, unsigned aDestID)const;
  bool GetEdgeStatusAttributeList(unsigned aSrcID, unsigned aDestID, unsigned aAttributeID)const;
  vector<unsigned> GetVertexAdjacentList(unsigned aID)const;
  vector<unsigned> GetEdgeAdjacentList(unsigned aID)const;
  unsigned GetEdgeID(unsigned aSrcID, unsigned aDestID)const;
  bool IsEdge(unsigned aSrcID, unsigned aDestID)const;
  ostream& Output(ostream& out)const;
  string Serialize()const;
  unsigned VertexSize()const;
  unsigned EdgeSize()const;
  bool IsEmpty()const;
protected:
  mutable bool mTopologicalChangeOccurrence;

  unsigned mVertexSize;
  unsigned mEdgeSize;

  vector<string> mVertexSymbolicIDList;
  vector< vector<EdgeClass> > mAdjacencyList;

  vector<vector<double> > mVertexNumericAttributeList; 
  vector<vector<string> > mVertexSymbolicAttributeList; 
  vector<vector<bool> > mVertexStatusAttributeList; 

  vector<vector<double> > mEdgeNumericAttributeList; 
  vector<vector<string> > mEdgeSymbolicAttributeList; 
  vector<vector<bool> > mEdgeStatusAttributeList; 
};








#endif
