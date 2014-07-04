/// -*- c++ -*-
/// @file   GraphClass.h
/// @author Fabrizio Costa <costa@informatik.uni-freiburg.de>
/// @brief  Graph class used to compute the NSPDK

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

#ifndef GRAPHCLASS_H
#define GRAPHCLASS_H

#include "Utility.h"
#include "BaseGraphClass.h"


//CONSTANTS
/**
   Definition of offsets in the STATUS vector for vertices and their
   semantic meaning.
*/
const unsigned LABEL_VERTEX_ATTRIBUTE_ID=0;
const unsigned KERNEL_POINT_ID=0;
const unsigned KIND_ID=1;
const unsigned VIEWPOINT_ID=2;
const unsigned DEAD_ID=3;
/**
   Definition of offsets in the STATUS vector for edges and their
   semantic meaning.
*/
const unsigned EDGE_ATTRIBUTE_ID=0;
const unsigned EDGE_DEAD_ID=0;


class GraphClass:public BaseGraphClass {
  friend ostream& operator<<(ostream& out, const GraphClass& aG);
private:
  bool simplified_export;
public:
  GraphClass(const string& aGid="temp"):
    mGraphID(aGid),mSliceIdList(1),mMaxRadius(-1),mMaxDistance(-1)
  { }
  string GetGraphID()const;
  vector<unsigned> GetVertexAdjacentList(unsigned aID)const;
  bool IsSliced(void) const {return mSliceIDs.size() >= 2;}
  ostream& Output(ostream& out)const;
  void ExportGraph(const string& aFilename, const string& aFormat);

  void SetSliceID(unsigned aID, const std::string& aSliceID);
  string GetSliceID(unsigned aID) const;
  void SetVertexKernelPoint(unsigned aID, bool state);
  bool GetVertexKernelPoint(unsigned aID) const ;
  void SetVertexKind(unsigned aID, bool kind);
  bool GetVertexKind(unsigned aID) const ;
  void SetVertexViewPoint(unsigned aID, bool state);
  bool GetVertexViewPoint(unsigned aID) const ;
  void SetVertexAlive(unsigned aID, bool state);
  bool GetVertexAlive(unsigned aID) const ;
  void SetSignatureAliveness(std::string s, bool state);
  void SetSliceAliveness(std::string slice_id, bool state);
  void SetSignatureInSliceAliveness(std::string sig,std::string slice_id, bool state);
  void SetAllAliveness(bool state);
  void SetVertexDead(unsigned aID, bool state);
  bool GetVertexDead(unsigned aID) const ;
  void SetVertexLabel(unsigned aID, const string& aLabel);
  string GetVertexLabel(unsigned aID)const;
  string GetEdgeLabel(unsigned aSrcID, unsigned aDestID)const;
  void SetEdgeLabel(unsigned aSrcID, unsigned aDestID, const string& aLabel);
  bool Check()const;
  void ComputePairwiseDistanceInformation(int aMaxDistance=-1,int aMaxRadius=-1)const;
  void clean_internals(void);

  vector<unsigned> GetFixedDistanceVertexIDList(unsigned aSrcID, int aDistance)const;
  int PairwiseDistance(unsigned aSrcID, unsigned aDestID)const;
  vector<unsigned> GetNeighborhoodVertexIDList(unsigned aSrcID, unsigned aRadius) const;
  void SaveAsMatlabFile(const string& aFilename, vector<unsigned>& aViewPointList)const;

protected:
  void SingleVertexBoundedBreadthFirstVisit(unsigned aRootVertexIndex, int aRadius, map<pair<unsigned,unsigned>,int>& oSrcDestMaptoDistance)const;
  string vertex_label_serialize(unsigned v, string separator="\n") const;
  string edge_label_serialize(unsigned edge_id) const;
  void SaveAsDotFile(const string& aFilename)const;
  void SaveAsGMLFile(const string& aFilename)const;
  void SaveAsGDLFile(const string& aFilename)const;
  void SaveAsCSVFile(const string& aFilename)const;
  void SaveAsGspanFile(const string& aFilename)const;
  void SaveGspanViewFile(const string& aFilename, vector<unsigned>& aViewPointList) const;

protected:
  string mGraphID;
  vector<string> mSliceIdList;
  std::set<std::string> mSliceIDs;
  mutable int mMaxRadius;
  mutable int mMaxDistance;

  mutable map<pair<unsigned,unsigned>,int> mSrcDestMaptoDistance;//semantic: <src_id,dest_id> \mapsto distance
  mutable map<pair<unsigned,int>,vector<unsigned> > mSrcDistanceMaptoDestList;//semantic: <src_id,distance> \mapsto list of dest_id
};
#endif





