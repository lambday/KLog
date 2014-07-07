/// -*- c++ -*-
/// @file   NSPDK_FeatureGenerator.h
/// @author Fabrizio Costa <costa@informatik.uni-freiburg.de>
/// @brief  Compute the NSPDK feature vectors

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

#ifndef NSPDK_FEATUREGENERATOR_H
#define NSPDK_FEATUREGENERATOR_H

#include "Utility.h"
#include "FlagsService.h"
#include "GraphClass.h"
#include "Histogram.h"
#include "FeatureGenerator.h"
#include "Hash.h"

using namespace std;

struct DebugClass {
    void Clear();
    void Output(ostream& out)const;
    void OutputGraphEncoding(ostream& out)const;
    void OutputFeatureEncoding(ostream& out)const;
    void OutputFunctorEncoding(ostream& out)const;
    void OutputBinEncoding(ostream& out)const;
    void OutputPlainEncoding(ostream& out)const;

    map<unsigned, string> mHashToPlainFeatureMap;
    map<unsigned, string> mHashToPlainSubgraphMap;
    map<unsigned, pair<unsigned, unsigned> > mHashToPlainSubgraphPairMap;
    map<unsigned, string> mHashToRadiusDistanceMap;
    map<unsigned, string> mHashToPredicateMap;
    map<unsigned, string> mHashToBinMap;

    multimap<string, string> mEncodingToPlainNeighborhoodMap;
};

class NSPDK_FeatureGenerator : public FeatureGenerator, public FlagsServiceClient {
public:
    NSPDK_FeatureGenerator(const std::string& id);
    virtual void generate_feature_vector(const GraphClass& aG, CaseWrapper* x, const vector<unsigned>& aFirstEndpointList = vector<unsigned>());
    // virtual double compute_kernel(const GraphClass& aG, const GraphClass& aH, const vector<unsigned>& aGFirstEndpointList=vector<unsigned>(), const vector<unsigned>& aHFirstEndpointList=vector<unsigned>());
    void Output(ostream& out)const;
    void OutputFeatureMap(ostream& out)const;
    virtual void Clear();

    string get_param(const string& name) {
        if (name == "radius")
            return stream_cast<std::string>(mRadius);
        else if (name == "distance")
            return stream_cast<std::string>(mDistance);
        else
            KLOG_THROW("Invalid name" << name);
    }

    unsigned get_distance() const {
        return mDistance;
    }
protected:
    typedef pair<unsigned, unsigned> EdgeType;
    typedef map<unsigned, string> VertexToLabelMapType;
    typedef map<EdgeType, string> EdgeToLabelMapType;

    void ConvertToSparseVector(CaseWrapper* x) const;
    void GetFirstEndpoints(const GraphClass& aG, vector<unsigned>& oFirstEndpointRootId)const;
    virtual void GenerateFeatures(const GraphClass& aG, const vector<unsigned>& aFirstEndpointList = vector<unsigned>());
    virtual void GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList);
    pair<unsigned, SecondOrderHistogramClass > BoundedRadiusRootedGraphCanonicalForm(int aRootVertexIndex, const GraphClass& aG, int aRadius);
    unsigned HardEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius);
    SecondOrderHistogramClass SoftEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius);
    string GetVertexLabel(const GraphClass& aG, unsigned aVertexID)const;
    string GetEdgeLabel(const GraphClass& aG, unsigned aSrcVertexID, unsigned aDestVertexID)const;
    void InsertFeature(const string& aLabel, int aRootVertexIndex, const GraphClass& aG, SecondOrderHistogramClass& oSoftAttributeList);
    unsigned Radius0RootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG);
    SecondOrderHistogramClass Radius0RootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG);
    unsigned Radius1RootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG);
    SecondOrderHistogramClass Radius1RootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG);
    virtual unsigned RadiusKRootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius);
    virtual SecondOrderHistogramClass RadiusKRootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG, int aRadius);

protected:
    unsigned mRadius;
    unsigned mDistance;
    string mMatchType;
    unsigned mHashBitSize;
    unsigned mBitMask;
    bool mMinKernel;
    bool mNormalization;
    unsigned mDebugVerbosity;

    map<unsigned, ThirdOrderHistogramClass> mFeatureList;
    map<pair<unsigned, unsigned>, pair<unsigned, SecondOrderHistogramClass > > mSubgraphEncodingCache; //semantic: <radius,vertex_id> \mapsto <hash_code,soft_attribute_histogram>

    mutable VertexToLabelMapType mVertexToLabelMap;
    mutable EdgeToLabelMapType mEdgeToLabelMap;

    mutable DebugClass mDebugInfo;
};

//----------------------------------------------------------------------------------------------------------------------------------
// Approximate NSPDK

class ANSPDK_FeatureGenerator : public NSPDK_FeatureGenerator {
public:
    ANSPDK_FeatureGenerator(const std::string& id);
protected:
    virtual unsigned RadiusKRootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius);
    virtual SecondOrderHistogramClass RadiusKRootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG, int aRadius);
};

//----------------------------------------------------------------------------------------------------------------------------------
//3D NSPDK (suitable for molecules)

class NSPDK3D_FeatureGenerator : public ANSPDK_FeatureGenerator {
public:
    NSPDK3D_FeatureGenerator(const std::string& id);
    virtual void Clear();
    virtual void GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList);
    vector<double> GetRootDirection(int aRootVertexIndex, const GraphClass& aG, int aRadius)const;
    vector<double> GetDirection(int aSrcVertexIndex, int aDestVertexIndex, const GraphClass& aG)const;
    unsigned ComputeDirectionAgreement(const vector<double>& aSrcDir, const vector<double>& aDestDir)const;
    vector<double> ComputeAverageDirection(const vector<vector<double> >& aDirectionList)const;
    double ComputeVectorNorm(const vector<double>& aVec)const;
public:
    map<pair<unsigned, unsigned>, vector<double> > mRootDirectionEncodingCache; //semantic: <radius,vertex_id> \mapsto 3D coordinates
};

//----------------------------------------------------------------------------------------------------------------------------------
// Memoized NSPDK
// feature information is stored for each graph-vertex pair; this saves computation time when only viewpoints are changed across feature generation invocations 

class MNSPDK_FeatureGenerator : public NSPDK_FeatureGenerator {

    struct SubgraphEncodingCacheIndex {

        bool operator<(const SubgraphEncodingCacheIndex& aB)const {
            const SubgraphEncodingCacheIndex& A = *this;
            if (A.mpGraph < aB.mpGraph) return true;
            else if (A.mpGraph == aB.mpGraph && A.mRadius < aB.mRadius) return true;
            else if (A.mpGraph == aB.mpGraph && A.mRadius == aB.mRadius && A.mVertexID < aB.mVertexID) return true;
            else return false;
        }
        const BaseGraphClass* mpGraph;
        unsigned mRadius;
        unsigned mVertexID;
    };
public:
    MNSPDK_FeatureGenerator(const std::string& id);
    virtual void Clear();
protected:
    virtual void GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList);
protected:
    map<SubgraphEncodingCacheIndex, pair<unsigned, SecondOrderHistogramClass > > mMemoizedSubgraphEncodingCache; //semantic: <*graph,radius,vertex_id> \mapsto <hash_code,soft_attribute_histogram>
};

//----------------------------------------------------------------------------------------------------------------------------------
//Relational NSPDK
//vertex label of viewpoints is marked; this ensures that viewpoint identity influences the feature representation 

class RNSPDK_FeatureGenerator : public NSPDK_FeatureGenerator {
public:
    RNSPDK_FeatureGenerator(const std::string& id);
    virtual void GenerateFeatures(const GraphClass& aG, const vector<unsigned>& aFirstEndpointList = vector<unsigned>());
};

//----------------------------------------------------------------------------------------------------------------------------------
//ALias NSPDK
//the neighborhood of the first endpoint is discarded: this allows different types of vertices to be aliased as they become distinguishable only via their neighborhood

class ALNSPDK_FeatureGenerator : public ANSPDK_FeatureGenerator {
public:
    ALNSPDK_FeatureGenerator(const std::string& id);
    virtual void GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList);
};

//----------------------------------------------------------------------------------------------------------------------------------
//Gapped NSPDK
//the distance information is discarded: pairs of neighbourhood subgraphs at different distances are encoded identically

class GNSPDK_FeatureGenerator : public NSPDK_FeatureGenerator {
public:
    GNSPDK_FeatureGenerator(const std::string& id);
    virtual void GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList);
};

#endif /* NSPDK_FEATUREGENERATOR_H */
