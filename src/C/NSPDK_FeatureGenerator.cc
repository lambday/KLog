/// -*- c++ -*-
/// @file   NSPDK_FeatureGenerator.cc
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

#include "NSPDK_FeatureGenerator.h"

#include "Dataset.h"

class Cleanup_Vectors: public SetFlagHook
{
  virtual void call(void) {The_Dataset.cleanup_vectors();}
};

void DebugClass::Clear(){
  mHashToPlainSubgraphMap.clear();
  mHashToPlainSubgraphPairMap.clear();
  mHashToRadiusDistanceMap.clear();
  mHashToPredicateMap.clear();
  mHashToBinMap.clear();
  mEncodingToPlainNeighborhoodMap.clear();
}

void DebugClass::Output(ostream& out)const{
  OutputFeatureEncoding(out);
  OutputGraphEncoding(out);
  OutputFunctorEncoding(out);
  OutputBinEncoding(out);
  //OutputPlainEncoding(out);
}

void DebugClass::OutputGraphEncoding(ostream& out)const{
  out<<"#Neighborhood subgraphs: ["<<mEncodingToPlainNeighborhoodMap.size()<<"]"<<endl;
  for (multimap<string,string>::const_iterator it=mEncodingToPlainNeighborhoodMap.begin();it!=mEncodingToPlainNeighborhoodMap.end();++it)
    out<<it->first<<" "<<it->second<<endl;
}

void DebugClass::OutputFeatureEncoding(ostream& out)const{
  out<<"#Feature encodings: ["<<mHashToPlainFeatureMap.size()<<"]"<<endl;
  for (map<unsigned,string>::const_iterator it=mHashToPlainFeatureMap.begin();it!=mHashToPlainFeatureMap.end();++it)
    out<<it->first<<" -> "<<it->second<<endl;
}

void DebugClass::OutputFunctorEncoding(ostream& out)const{
  out<<"#Predicate encodings: ["<<mHashToPredicateMap.size()<<"]"<<endl;
  for (map<unsigned,string>::const_iterator it=mHashToPredicateMap.begin();it!=mHashToPredicateMap.end();++it)
    out<<it->first<<" -> "<<it->second<<endl;
}

void DebugClass::OutputBinEncoding(ostream& out)const{
  out<<"#Histogram Bin encodings: ["<<mHashToBinMap.size()<<"]"<<endl;
  for (map<unsigned,string>::const_iterator it=mHashToBinMap.begin();it!=mHashToBinMap.end();++it)
    out<<it->first<<" -> "<<it->second<<endl;
}

void DebugClass::OutputPlainEncoding(ostream& out)const{
  out<<"#Plain encodings: ["<<mEncodingToPlainNeighborhoodMap.size()<<"]"<<endl;
  for (map<string,string>::const_iterator it=mEncodingToPlainNeighborhoodMap.begin();it!=mEncodingToPlainNeighborhoodMap.end();++it)
    out<<it->first<<" -> "<<it->second<<endl;
}

//----------------------------------------------------------------------------------------------------------------------------
NSPDK_FeatureGenerator::NSPDK_FeatureGenerator(const std::string& id): FeatureGenerator(id),FlagsServiceClient(id) {
    // Defaults are basically MLN features...
    mRadius = 0;
    mDistance = 0;
    mMatchType = "hard";
    mHashBitSize = (unsigned)(numeric_limits<unsigned>::digits-1);
    mBitMask = numeric_limits<unsigned>::max()>>1;
    mMinKernel = false;
    mNormalization=true;
    mDebugVerbosity=0;
    // pass the following hook to new_flag() if changing the flag
    // affects feature vectors: feature vectors will be cleaned up
    // (and regenerated if needed) if any of these flags is touched.
    Cleanup_Vectors* hook = new Cleanup_Vectors();
    new_flag(&mRadius, "radius","(unsigned)\nMax radius of kernel neighborhoods",hook);
    new_flag(&mDistance, "distance","(unsigned)\nMax distance between pairs of neighborhoods",hook);
    new_flag(&mMatchType, "match_type","(string)\nHow to match neighborhoods: soft, hard, hard_propertyless, mixed, hard_soft, multiview",hook);
    new_flag(&mNormalization, "normalization","(bool)\nNormalize feature vectors",hook);
    new_flag(&mMinKernel, "min_kernel","(bool)\nApply the min-kernel on top of the generated features",hook);
    new_flag(&mHashBitSize, "hash_bit_size","(unsigned)\nNumber of bits for hash values",hook);//FIXME: since parameter variables are accessed directly the bit size is useless as setting it cannot trigger automatically the computation of the bit mask; the only solution is to set directly the bit mask itself
    new_flag(&mBitMask, "hash_bit_mask","(unsigned)\nMask for hash values",hook);
    new_flag(&mDebugVerbosity, "verbosity","(unsigned)\nNumber for debug versosity level");
  }

void NSPDK_FeatureGenerator::Clear(){
  mFeatureList.clear();
  mSubgraphEncodingCache.clear();
  mVertexToLabelMap.clear();
  mEdgeToLabelMap.clear();
  mDebugInfo.Clear();
}

void NSPDK_FeatureGenerator::generate_feature_vector(const GraphClass& aG, CaseWrapper* x, const vector<unsigned>& aFirstEndpointList) {
  NSPDK_FeatureGenerator::Clear();
  GenerateFeatures(aG,aFirstEndpointList);
  ConvertToSparseVector(x);
  if (mDebugVerbosity>1) cerr<<"Feature vector: "<<endl<<*x<<std::endl;
  if (mDebugVerbosity>2){
    // cerr<<"Hash Map:"<<endl;
    mDebugInfo.Output(cerr);
    // cerr<<"Vector encoding: "<<endl<<x;
  }
  if (mDebugVerbosity>3) cerr<<"Graph:"<<endl<<aG<<endl;
}

void NSPDK_FeatureGenerator::OutputFeatureMap(ostream& out)const{
  mDebugInfo.OutputFeatureEncoding(out);
}

// double NSPDK_FeatureGenerator::compute_kernel(const GraphClass& aG, const GraphClass& aH, const vector<unsigned>& aGFirstEndpointList, const vector<unsigned>& aHFirstEndpointList){
//   //TODO: store in cache the vector representation (exploit symmetry)
//   SVector x;
//   generate_feature_vector(aG,x,aGFirstEndpointList);
//   SVector z;
//   generate_feature_vector(aH,z,aHFirstEndpointList);
//   return dot(x,z);
// }


void NSPDK_FeatureGenerator::ConvertToSparseVector(CaseWrapper* x) const {
  const int NUM_HIERARCHICAL_LEVELS=5;
  vector<unsigned> feature_list(NUM_HIERARCHICAL_LEVELS);
  double norm_level0=mFeatureList.size();
  for (map<unsigned,ThirdOrderHistogramClass>::const_iterator mt=mFeatureList.begin();mt!=mFeatureList.end();++mt){//index over all pairs <radius-distance>
    unsigned data_level0_feature=mt->first;
    feature_list[0]=data_level0_feature; 
    const ThirdOrderHistogramClass& data_level1=mt->second;
    double norm_level1=data_level1.mThirdOrderHistogram.size();
    if (norm_level1>0){ 
      for(map<unsigned,SecondOrderHistogramClass>::const_iterator it=data_level1.mThirdOrderHistogram.begin();it!=data_level1.mThirdOrderHistogram.end();++it){//index over selectors
	unsigned data_level1_feature=it->first;
	feature_list[1]=data_level1_feature; 
	const SecondOrderHistogramClass& data_level2=it->second;
	double norm_level2=data_level2.mSecondOrderHistogram.size();
	for(map<unsigned,HistogramClass>::const_iterator jt=data_level2.mSecondOrderHistogram.begin();jt!=data_level2.mSecondOrderHistogram.end();++jt){//index over predicates
	  unsigned data_level2_feature=jt->first;
	  feature_list[2]=data_level2_feature; 
	  const HistogramClass& data_level3=jt->second;
	  double norm_level3=0;
	  if (mMinKernel){//simulate min kernel //FIXME:devise normalization strategy
	    for (map<unsigned,double>::const_iterator kt=data_level3.mHistogram.begin();kt!=data_level3.mHistogram.end();++kt){//index over bins
	      unsigned data_level3_feature=kt->first;
	      feature_list[3]=data_level3_feature;
	      double value=kt->second;
		for (unsigned z=0;z<value;z++) {
		  feature_list[4]=z;
		  unsigned feature_hash=HashFunc(feature_list,mBitMask);
		  x->set(feature_hash,1);
		}//simulate min kernel
	    }	    
	  } //end min kernel case
	  else {//linear case
	    double norm=1;
	    if (mNormalization) {
	      //normalized dot product
	      for (map<unsigned,double>::const_iterator kt=data_level3.mHistogram.begin();kt!=data_level3.mHistogram.end();++kt){//compute norm
		double value=kt->second;
		norm_level3 += value*value;
	      }
	      norm=sqrt(norm_level0*norm_level1*norm_level2*norm_level3);
	    }
	    for (map<unsigned,double>::const_iterator kt=data_level3.mHistogram.begin();kt!=data_level3.mHistogram.end();++kt){//index over bins
	      unsigned data_level3_feature=kt->first;
	      feature_list[3]=data_level3_feature;
	      unsigned feature_hash=HashFunc(feature_list,mBitMask);
	      double value=kt->second/norm;
	      x->set(feature_hash,value);

	      if (mDebugVerbosity>0) {
		string radius_distance_str=mDebugInfo.mHashToRadiusDistanceMap[data_level0_feature];
		pair<unsigned,unsigned> subgraph_pair=mDebugInfo.mHashToPlainSubgraphPairMap[data_level1_feature];
		string subgraph_a_str=mDebugInfo.mHashToPlainSubgraphMap[subgraph_pair.first];
		string subgraph_b_str=mDebugInfo.mHashToPlainSubgraphMap[subgraph_pair.second];
		string functor_str=mDebugInfo.mHashToPredicateMap[data_level2_feature];
		string bin_str=mDebugInfo.mHashToBinMap[data_level3_feature];
		string plain_str="<"+radius_distance_str+"> <"+subgraph_a_str+"> <"+subgraph_b_str+"> <"+functor_str+"> <"+bin_str+">";
		mDebugInfo.mHashToPlainFeatureMap[feature_hash]=plain_str;
	      }
	    }//bin
	  }//linear case	
	}//functors
      }//selectors
    }//if not empty
  }//radius-distance
}

void NSPDK_FeatureGenerator::GetFirstEndpoints(const GraphClass& aG, vector<unsigned>& oFirstEndpointList)const{
  if (oFirstEndpointList.size()==0){//if oFirstEndpointList is empty then fill it with all vertices that are viewpoints
    for (unsigned i=0;i<aG.VertexSize();++i)
      if (aG.GetVertexViewPoint(i)) oFirstEndpointList.push_back(i);
    if (oFirstEndpointList.size()==0){//if no viewpoints are set just fill it with all graph vertices
      for (unsigned i=0;i<aG.VertexSize();++i) 
	oFirstEndpointList.push_back(i);      
    }
  }
  if (mDebugVerbosity>3) {
    cout<<"First endpoint id list ["<<oFirstEndpointList.size()<<"]:"<<endl;	
    for (unsigned i=0;i<oFirstEndpointList.size();i++)
      cout<<oFirstEndpointList[i]<<" ";
    cout<<endl;
  }
}

void NSPDK_FeatureGenerator::GenerateFeatures(const GraphClass& aG, const vector<unsigned>& aFirstEndpointList){
  aG.ComputePairwiseDistanceInformation(mDistance,mRadius);
  vector<unsigned> first_endpoint_list=aFirstEndpointList;
  GetFirstEndpoints(aG,first_endpoint_list);
  if (first_endpoint_list.size()==0) throw std::logic_error("ERROR: Something went wrong: cannot generate features over an empty set of first endpoints!");
  if (aG.Check()==false) throw logic_error("Graph has not passed sanity check");  //check graph data structure soundness
  for (unsigned r=0;r<=mRadius;r++){
    for (unsigned d=0;d<=mDistance; d++){
      GenerateFeatures(aG,d,r,first_endpoint_list);
    }
  }
}

void NSPDK_FeatureGenerator::GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList){
  ThirdOrderHistogramClass feature_list;
  bool empty_flag=true;
  for (unsigned i=0;i<aFirstEndpointList.size();i++){
    unsigned src_id=aFirstEndpointList[i];
    if (aG.GetVertexKernelPoint(src_id) && aG.GetVertexAlive(src_id)){//proceed to extract features only if the *src* vertex is a kernel point and is alive
      pair<unsigned,unsigned> radius_src_id=make_pair(aRadius,src_id);
      pair<unsigned,SecondOrderHistogramClass> src_feature;
      if (mSubgraphEncodingCache.count(radius_src_id)==0){
	src_feature=BoundedRadiusRootedGraphCanonicalForm(src_id, aG, aRadius);
	mSubgraphEncodingCache[radius_src_id]=src_feature;
      } else {
	src_feature=mSubgraphEncodingCache[radius_src_id];	    
      }
      if (aDistance>0) {	
	vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(src_id,aDistance);
	for (unsigned dest_j=0;dest_j<dest_id_list.size();dest_j++){
	  unsigned dest_id=dest_id_list[dest_j];
	  if (aG.GetVertexKernelPoint(dest_id) && aG.GetVertexAlive(dest_id)){//proceed to extract features only if the *dest* vertex is a kernel point and is alive
	    pair<unsigned,unsigned> radius_dest_id=make_pair(aRadius,dest_id);
	    pair<unsigned,SecondOrderHistogramClass> dest_feature;
	    if (mSubgraphEncodingCache.count(radius_dest_id)==0){
	      dest_feature=BoundedRadiusRootedGraphCanonicalForm(dest_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_dest_id]=dest_feature;
	    }
	    else dest_feature=mSubgraphEncodingCache[radius_dest_id];
	    vector<unsigned> endpoint_list;
	    if (src_feature.first<dest_feature.first) {
	      endpoint_list.push_back(src_feature.first);
	      endpoint_list.push_back(dest_feature.first);
	    } else {
	      endpoint_list.push_back(dest_feature.first);
	      endpoint_list.push_back(src_feature.first);
	    }
	    unsigned key=HashFunc(endpoint_list,mBitMask);
	    SecondOrderHistogramClass src_soft_feature=src_feature.second;
	    SecondOrderHistogramClass dest_soft_feature=dest_feature.second;
	    src_soft_feature.Add(dest_soft_feature);
	    feature_list.Add(key,src_soft_feature);
	    empty_flag=false;
	    if (mDebugVerbosity>0) {
	      if (src_feature.first<dest_feature.first) 
		mDebugInfo.mHashToPlainSubgraphPairMap[key]=make_pair(src_feature.first,dest_feature.first);
	      else
		mDebugInfo.mHashToPlainSubgraphPairMap[key]=make_pair(dest_feature.first,src_feature.first);
	    }
	  }//if dest is of active type
	}//for dest_id
      }//if dist > 0
      else {//dist=0 then use only src encoding
	vector<unsigned> endpoint_list;
	endpoint_list.push_back(src_feature.first);
	unsigned key=HashFunc(endpoint_list,mBitMask);
	SecondOrderHistogramClass src_soft_feature=src_feature.second;
	feature_list.Add(key,src_soft_feature);
	empty_flag=false;
	if (mDebugVerbosity>0) {
	  mDebugInfo.mHashToPlainSubgraphPairMap[key]=make_pair(src_feature.first,src_feature.first);
	}
      }
    }//if src is active
  }//for src_id
  if (empty_flag){}
  else {
    vector<unsigned> distance_radius_list;
    distance_radius_list.push_back(aDistance);
    distance_radius_list.push_back(aRadius);
    unsigned distance_radius_hash=HashFunc(distance_radius_list,mBitMask);
    mFeatureList[distance_radius_hash]=feature_list;
    if (mDebugVerbosity>0) mDebugInfo.mHashToRadiusDistanceMap[distance_radius_hash]="d:"+stream_cast<string>(aDistance)+" r:"+stream_cast<string>(aRadius);
  }
}

pair<unsigned,SecondOrderHistogramClass > NSPDK_FeatureGenerator::BoundedRadiusRootedGraphCanonicalForm(int aRootVertexIndex, const GraphClass& aG, int aRadius){
    unsigned discrete_encoding=0;
    SecondOrderHistogramClass soft_encoding;
    if (mMatchType == "hard" || mMatchType == "hard_propertyless" || mMatchType == "multiview"){
      discrete_encoding=HardEncoding(aRootVertexIndex,aG,aRadius);
      soft_encoding.Insert(1,1);
    } else if (mMatchType == "soft" || mMatchType == "multiview"){
      discrete_encoding=HardEncoding(aRootVertexIndex,aG,0);
      soft_encoding=SoftEncoding(aRootVertexIndex,aG,aRadius);
    } else if (mMatchType == "hard_soft"){
      discrete_encoding=HardEncoding(aRootVertexIndex,aG,aRadius);
      soft_encoding=SoftEncoding(aRootVertexIndex,aG,aRadius);
    } else if (mMatchType == "mixed"){
      discrete_encoding=HardEncoding(aRootVertexIndex,aG,aRadius);
      soft_encoding=SoftEncoding(aRootVertexIndex,aG,mRadius);//NOTE:here it is always the maximum mRadius for the soft encoding
    } else throw range_error("Non managed match type: "+stream_cast<string>(mMatchType));
    return make_pair(discrete_encoding, soft_encoding); 
  }


string NSPDK_FeatureGenerator::GetVertexLabel(const GraphClass& aG, unsigned aVertexID)const{
  VertexToLabelMapType::iterator cachedLabelEntry = mVertexToLabelMap.find(aVertexID);
  if (cachedLabelEntry != mVertexToLabelMap.end()) return cachedLabelEntry->second;
  else {
    if (mMatchType == "hard"){
      string label;
      const vector<string>& symbolic_attribute_list=aG.GetVertexSymbolicAttributeListRef(aVertexID);
      if (symbolic_attribute_list.size()>0) label=symbolic_attribute_list[0];
      for (unsigned i=1;i<symbolic_attribute_list.size();i++)
	label+="."+symbolic_attribute_list[i];
      mVertexToLabelMap[aVertexID]=label;
      return label;
    } else   if (mMatchType == "hard_propertyless" || mMatchType == "soft" || mMatchType == "hard_soft" || mMatchType == "multiview"  || mMatchType == "mixed"){
      string label=aG.GetVertexLabel(aVertexID);
      mVertexToLabelMap[aVertexID]=label;
      return label;
    } else throw range_error("Non managed match type: "+stream_cast<string>(mMatchType));
  }
}

string NSPDK_FeatureGenerator::GetEdgeLabel(const GraphClass& aG, unsigned aSrcVertexID, unsigned aDestVertexID)const{
  pair<unsigned,unsigned> edge=make_pair(aSrcVertexID,aDestVertexID);
  EdgeToLabelMapType::iterator cachedLabelEntry = mEdgeToLabelMap.find(edge);
  if (cachedLabelEntry != mEdgeToLabelMap.end()) return cachedLabelEntry->second;
  else {
    if (mMatchType == "hard"){
      string label;
      vector<string> symbolic_attribute_list=aG.GetEdgeSymbolicAttributeList(aSrcVertexID,aDestVertexID);
      if (symbolic_attribute_list.size()>0) label=symbolic_attribute_list[0];
      for (unsigned i=1;i<symbolic_attribute_list.size();i++)
	label+="."+symbolic_attribute_list[i];
      mEdgeToLabelMap[edge]=label;
      return label;
    } else   if (mMatchType == "hard_propertyless" || mMatchType == "soft" || mMatchType == "hard_soft" || mMatchType == "multiview" || mMatchType == "mixed"){
      string label=aG.GetEdgeLabel(aSrcVertexID,aDestVertexID);
      mEdgeToLabelMap[edge]=label;
      return label;
    } else throw range_error("Non managed match type: "+stream_cast<string>(mMatchType));
  } 
}

unsigned NSPDK_FeatureGenerator::HardEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius){
    //NOTE:for efficiency reasons case radius=0 and radius=1 are treated as special cases
    unsigned discrete_encoding;
    if (aRadius==0) { //return root label
      discrete_encoding=Radius0RootedGraphCanonicalFormEncoding(aRootVertexIndex,aG);
    } else if (aRadius==1){ //return the sorted sequence of root's children
      discrete_encoding=Radius1RootedGraphCanonicalFormEncoding(aRootVertexIndex,aG);
    } else { //general case
      discrete_encoding=RadiusKRootedGraphCanonicalFormEncoding(aRootVertexIndex,aG,aRadius);
    }

    //store plain graph to feature map
    if (mDebugVerbosity>0){
      //extract set of vertices in the ball of radius aMaxDepth
      set<unsigned> ball;
      for (int r=0;r<=aRadius;r++) {
	vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(aRootVertexIndex,r);
	ball.insert(dest_id_list.begin(),dest_id_list.end());
      }
      
      //induce the subgraph from the ball and return the new index for the root vertex
      GraphClass gal;
      unsigned root=aG.GetVertexInducedRootedSubGraph(ball,aRootVertexIndex,gal);
      string root_label=gal.GetVertexLabel(root);
      root_label+="*";
      gal.SetVertexLabel(root,root_label);
      string attribute="graph_id: "+aG.GetGraphID()+
	" vertex_id: "+stream_cast<string>(aRootVertexIndex)+
	" radius: "+stream_cast<string>(aRadius)+" "+
	" hash_value: "+stream_cast<string>(discrete_encoding);
      string value="plain_graph: "+gal.Serialize();
      mDebugInfo.mEncodingToPlainNeighborhoodMap.insert(make_pair(attribute,value));
      mDebugInfo.mHashToPlainSubgraphMap[discrete_encoding]=value;
    }

    return discrete_encoding;
  }

SecondOrderHistogramClass NSPDK_FeatureGenerator::SoftEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius){
    //NOTE:for efficiency reasons case radius=0 and radius=1 are treated as special cases
    SecondOrderHistogramClass soft_encoding;
    if (aRadius==0) { //return root label
      soft_encoding=Radius0RootedGraphCanonicalFormAttributeList(aRootVertexIndex,aG);
    } else if (aRadius==1){ //return the sorted sequence of root's children
      soft_encoding=Radius1RootedGraphCanonicalFormAttributeList(aRootVertexIndex,aG);      
    } else { //general case
      soft_encoding=RadiusKRootedGraphCanonicalFormAttributeList(aRootVertexIndex,aG,aRadius);      
    }
    return soft_encoding;
  }

void NSPDK_FeatureGenerator::InsertFeature(const string& aLabel, int aRootVertexIndex, const GraphClass& aG, SecondOrderHistogramClass& oSoftAttributeList){
    //symbolic attributes
    vector<string> symbolic_attribute_list=aG.GetVertexSymbolicAttributeList(aRootVertexIndex);
    for (unsigned k=0;k<symbolic_attribute_list.size();k++){
      string predicate="pred:"+aLabel;
      string bin="bin:"+stream_cast<string>(k)+"_v:"+stream_cast<string>(symbolic_attribute_list[k]);
      unsigned hash_predicate=HashFunc(predicate,mBitMask);
      unsigned hash_bin=HashFunc(bin,mBitMask);
      oSoftAttributeList.Insert(hash_predicate,hash_bin);
      if (mDebugVerbosity>0) {
	mDebugInfo.mHashToPredicateMap[hash_predicate]=predicate;
	mDebugInfo.mHashToBinMap[hash_bin]=bin;
      }
    }
    //numeric attributes
    vector<double> numeric_attribute_list=aG.GetVertexNumericAttributeList(aRootVertexIndex);
    for (unsigned k=0;k<numeric_attribute_list.size();k++){
      string functor="f:"+aLabel;
      string bin="bn:"+stream_cast<string>(k);
      unsigned hash_functor=HashFunc(functor,mBitMask);
      unsigned hash_bin=HashFunc(bin,mBitMask);
      oSoftAttributeList.Insert(hash_functor,hash_bin,numeric_attribute_list[k]);
      if (mDebugVerbosity>0) {
	mDebugInfo.mHashToPredicateMap[hash_functor]=functor;
	mDebugInfo.mHashToBinMap[hash_bin]=bin;
      }
    }
  }

unsigned NSPDK_FeatureGenerator::Radius0RootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG){
  string encoding=GetVertexLabel(aG,aRootVertexIndex);
  unsigned hash_subgraph=HashFunc(encoding,mBitMask);
 
  return hash_subgraph;
}

SecondOrderHistogramClass NSPDK_FeatureGenerator::Radius0RootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG){
    SecondOrderHistogramClass soft_attribute_list;
    string label=GetVertexLabel(aG,aRootVertexIndex);
    InsertFeature(label,aRootVertexIndex,aG,soft_attribute_list);
    return soft_attribute_list;  
  }

unsigned NSPDK_FeatureGenerator::Radius1RootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG){
    string encoding;
    encoding=GetVertexLabel(aG,aRootVertexIndex)+":";
    vector<pair<string,unsigned> > vertex_label_id_list;
    vector<unsigned> adjacency_list=aG.GetVertexAdjacentList(aRootVertexIndex);
    for (unsigned i=0;i<adjacency_list.size();++i){
      unsigned child_id=adjacency_list[i];
      string child_label=GetVertexLabel(aG,child_id)+"-"+GetEdgeLabel(aG,aRootVertexIndex,child_id);
      vertex_label_id_list.push_back(make_pair(child_label,child_id));
    }
    sort(vertex_label_id_list.begin(),vertex_label_id_list.end());
    if (vertex_label_id_list.size()>0)
      encoding+=vertex_label_id_list[0].first;
    for (unsigned i=1;i<vertex_label_id_list.size();i++)
      encoding+="."+vertex_label_id_list[i].first;
    unsigned hash_subgraph=HashFunc(encoding,mBitMask);
   
    return hash_subgraph;
  }

SecondOrderHistogramClass  NSPDK_FeatureGenerator::Radius1RootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG){
    SecondOrderHistogramClass soft_attribute_list;
    vector<pair<string,unsigned> > vertex_label_id_list;
    vector<unsigned> adjacency_list=aG.GetVertexAdjacentList(aRootVertexIndex);
    for (unsigned i=0;i<adjacency_list.size();++i){
      unsigned child_id=adjacency_list[i];
      string child_label=GetVertexLabel(aG,child_id)+"-"+GetEdgeLabel(aG,aRootVertexIndex,child_id);
      vertex_label_id_list.push_back(make_pair(child_label,child_id));
    }
    sort(vertex_label_id_list.begin(),vertex_label_id_list.end());
    for (unsigned i=0;i<vertex_label_id_list.size();i++){//for each child
      unsigned vertex_id=vertex_label_id_list[i].second;
      string label=stream_cast<string>(GetVertexLabel(aG,vertex_id));
      if (mMatchType == "hard_soft")
	label+="+pos"+stream_cast<string>(i); //NOTE:the unique encoding for radius 1 corresponds to the label+the sorted position of the adjacent vertex
      else {} 
      InsertFeature(label,vertex_id,aG,soft_attribute_list);
    }//end for each child
    //add root vertex info
    string label=stream_cast<string>(GetVertexLabel(aG,aRootVertexIndex))+"+r";
    InsertFeature(label,aRootVertexIndex,aG,soft_attribute_list);
    return soft_attribute_list;  
  }

unsigned NSPDK_FeatureGenerator::RadiusKRootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius){
  //extract set of vertices in the ball of radius aMaxDepth
    set<unsigned> ball;
    for (int r=0;r<=aRadius;r++) {
      vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(aRootVertexIndex,r);
      ball.insert(dest_id_list.begin(),dest_id_list.end());
    }
      
    //induce the subgraph from the ball and return the new index for the root vertex
    GraphClass gal;
    unsigned root=aG.GetVertexInducedRootedSubGraph(ball,aRootVertexIndex,gal);
    gal.ComputePairwiseDistanceInformation(aRadius*2);

    //for all vertices in ball extract the vertex's signature: distance from root-sorted distance from all the other vertices + their vertex label
    vector<unsigned> vertex_encoding_list;
    for(unsigned i=0;i<gal.VertexSize();++i){
      vector<unsigned> vertex_encoding;
      //distance from root
      vertex_encoding.push_back(gal.PairwiseDistance(root,i));
      vector<unsigned> distance_list;
      for(unsigned j=0;j<gal.VertexSize();++j){
	int dist=gal.PairwiseDistance(i,j);
	string distance_label=stream_cast<string>(dist)+gal.GetVertexLabel(j);
	unsigned hash_distance_label=HashFunc(distance_label,mBitMask);
	distance_list.push_back(hash_distance_label);
      }
      sort(distance_list.begin(),distance_list.end());
      vector<unsigned> sorted_vertex_encoding;
      for (unsigned t=0;t<distance_list.size();t++)
	sorted_vertex_encoding.push_back(distance_list[t]);
      unsigned hash_encoding=HashFunc(sorted_vertex_encoding,mBitMask);
      vertex_encoding_list.push_back(hash_encoding);
    }
    //extract list of all edge's signatures in the induced graph: v-signature,u-signature,label(uv)
    vector<unsigned> edge_list;
    for(unsigned u=0;u<gal.VertexSize();++u){
    
      //get all edges of vertex u
      vector<unsigned> adjacency_list=gal.GetVertexAdjacentList(u);
      for (unsigned j=0;j<adjacency_list.size();++j){
	unsigned v=adjacency_list[j];
	vector<unsigned> edge_encoding;
	if (vertex_encoding_list[u]<vertex_encoding_list[v]){
	  edge_encoding.push_back(vertex_encoding_list[u]);
	  edge_encoding.push_back(vertex_encoding_list[v]);
	} else {
	  edge_encoding.push_back(vertex_encoding_list[v]);
	  edge_encoding.push_back(vertex_encoding_list[u]);
	}
	unsigned hash_edge_label=HashFunc(gal.GetEdgeLabel(u,v),mBitMask);
	edge_encoding.push_back(hash_edge_label);
	unsigned hash_edge_encoding=HashFunc(edge_encoding,mBitMask);
	edge_list.push_back(hash_edge_encoding);
      }
   
    }
    //the graph encoding is the sorted list of edge encodings
    sort(edge_list.begin(),edge_list.end());
    unsigned hash_subgraph=HashFunc(edge_list,mBitMask);
    return hash_subgraph;
  }

SecondOrderHistogramClass NSPDK_FeatureGenerator::RadiusKRootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG, int aRadius){
    SecondOrderHistogramClass soft_attribute_list;
    //extract set of vertices in the ball of radius aRadius
    set<unsigned> ball; 
    for (int r=0;r<=aRadius;r++) {
      vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(aRootVertexIndex,r);
      ball.insert(dest_id_list.begin(),dest_id_list.end());
    }
      
    if (mMatchType == "soft"){
      //NOTE: in this case it is enough to extract all attributes from the vertices in the ball of radius aRadius
      for (set<unsigned>::iterator it=ball.begin(); it!=ball.end();++it){
	unsigned vertex_id=*it;
	string label=GetVertexLabel(aG,vertex_id);
	InsertFeature(label,vertex_id,aG,soft_attribute_list);
      }
    } else {
      //induce the subgraph from the ball
      GraphClass gal;
      unsigned root=aG.GetVertexInducedRootedSubGraph(ball,aRootVertexIndex,gal);
      gal.ComputePairwiseDistanceInformation(aRadius*2);
      
      //for all vertices in ball extract the vertex's signature: distance from root-sorted distance from all the other vertices + their vertex label      
      for(unsigned i=0;i<gal.VertexSize();++i){
	vector<unsigned> encoding;
	//distance from root
	encoding.push_back(gal.PairwiseDistance(root,i));
	vector<unsigned> distance_list;
	for(unsigned j=0;j<gal.VertexSize();++j){
	  int dist=gal.PairwiseDistance(i,j);
	  string distance_label=stream_cast<string>(dist)+gal.GetVertexLabel(j);
	  unsigned hash_distance_label=HashFunc(distance_label,mBitMask);
	  distance_list.push_back(hash_distance_label);
	}
	sort(distance_list.begin(),distance_list.end());
	for (unsigned t=0;t<distance_list.size();t++)
	  encoding.push_back(distance_list[t]);
	unsigned hash_encoding=HashFunc(encoding,mBitMask);
	InsertFeature(stream_cast<string>(hash_encoding),i,gal,soft_attribute_list);
      }
    }
    return soft_attribute_list;  
  }

void NSPDK_FeatureGenerator::Output(ostream& out)const{
  if (mDebugVerbosity>2){
    out<<"#Hierarchical data structure:"<<endl;
    for (map<unsigned,ThirdOrderHistogramClass>::const_iterator it=mFeatureList.begin();it!=mFeatureList.end();++it){
      unsigned radius_distance_id=it->first;
      const ThirdOrderHistogramClass& toh=it->second;
      out<<radius_distance_id<<" : "<<endl<<toh<<endl<<endl;
    }
  }
  mDebugInfo.Output(out);
}

//----------------------------------------------------------------------------------------------------------------------------

ANSPDK_FeatureGenerator::ANSPDK_FeatureGenerator(const std::string& id):NSPDK_FeatureGenerator(id){}

unsigned ANSPDK_FeatureGenerator::RadiusKRootedGraphCanonicalFormEncoding(int aRootVertexIndex, const GraphClass& aG, int aRadius){
  //extract set of vertices in the ball of radius aMaxDepth
    set<unsigned> ball;
    for (int r=0;r<=aRadius;r++) {
      vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(aRootVertexIndex,r);
      ball.insert(dest_id_list.begin(),dest_id_list.end());
    }
    //for all vertices in ball extract the vertex's signature: distance from root-sorted distance from all the other vertices + their vertex label
    vector<unsigned> vertex_encoding_list;
    for (set<unsigned>::const_iterator it=ball.begin();it!=ball.end();++it){
      unsigned i=*it;
      vector<unsigned> vertex_encoding;
      //distance from root
      vertex_encoding.push_back(aG.PairwiseDistance(aRootVertexIndex,i));
      vector<unsigned> distance_list;
      for (set<unsigned>::const_iterator jt=ball.begin();jt!=ball.end();++jt){
	unsigned j=*jt;
	int dist=aG.PairwiseDistance(i,j);	 
	string distance_label=stream_cast<string>(dist)+GetVertexLabel(aG,j);
	unsigned hash_distance_label=HashFunc(distance_label,mBitMask);
	distance_list.push_back(hash_distance_label);
      }
      sort(distance_list.begin(),distance_list.end());
      vector<unsigned> sorted_vertex_encoding;
      for (unsigned t=0;t<distance_list.size();t++)
	sorted_vertex_encoding.push_back(distance_list[t]);
      unsigned hash_vertex_encoding=HashFunc(sorted_vertex_encoding,mBitMask);
      vertex_encoding_list.push_back(hash_vertex_encoding);
    }
    //extract list of all edge's signatures in the induced graph: v-signature,u-signature,label(uv)
    vector<unsigned> edge_list;
    for (set<unsigned>::const_iterator it=ball.begin();it!=ball.end();++it){
      unsigned u=*it;
      
      //get all edges of vertex u
      vector<unsigned> adjacency_list=aG.GetVertexAdjacentList(u);
      for (unsigned j=0;j<adjacency_list.size();++j){
	unsigned v=adjacency_list[j];
	if (ball.count(v)>0){//if dest endpoint is in ball then add edge
	  vector<unsigned> edge_encoding;
	  if (vertex_encoding_list[u]<vertex_encoding_list[v]){
	    edge_encoding.push_back(vertex_encoding_list[u]);
	    edge_encoding.push_back(vertex_encoding_list[v]);
	  } else {
	    edge_encoding.push_back(vertex_encoding_list[v]);
	    edge_encoding.push_back(vertex_encoding_list[u]);
	  }
	  unsigned hash_edge_label=HashFunc(GetEdgeLabel(aG,u,v),mBitMask);
	  edge_encoding.push_back(hash_edge_label);
	  unsigned hash_edge_encoding=HashFunc(edge_encoding,mBitMask);
	  edge_list.push_back(hash_edge_encoding);
	}
      }
    }
    //graph encoding is sorted list of edge encodings
   
    sort(edge_list.begin(),edge_list.end());
    unsigned hash_subgraph=HashFunc(edge_list,mBitMask);
    if (mDebugVerbosity>0){
      //induce the subgraph from the ball and return the new index for the root vertex
      GraphClass gal;
      unsigned root=aG.GetVertexInducedRootedSubGraph(ball,aRootVertexIndex,gal);
      //mark the root and store the graph encoding
      string root_label=gal.GetVertexLabel(root);
      root_label+="*";
      gal.SetVertexLabel(root,root_label);
      string attribute="graph_id: "+aG.GetGraphID()+
	" vertex_id: "+stream_cast<string>(aRootVertexIndex)+
	" radius: "+stream_cast<string>(aRadius)+" "+
	" hash_value: "+stream_cast<string>(hash_subgraph);
      string value="plain_graph: "+gal.Serialize();
      mDebugInfo.mEncodingToPlainNeighborhoodMap.insert(make_pair(attribute,value));
      mDebugInfo.mHashToPlainSubgraphMap[hash_subgraph]=value;
    }
    return hash_subgraph;
  }

SecondOrderHistogramClass ANSPDK_FeatureGenerator::RadiusKRootedGraphCanonicalFormAttributeList(int aRootVertexIndex, const GraphClass& aG, int aRadius){
    SecondOrderHistogramClass soft_attribute_list;
    //extract set of vertices in the ball of radius aRadius
    set<unsigned> ball; 
    for (int r=0;r<=aRadius;r++) {
      vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(aRootVertexIndex,r);
      ball.insert(dest_id_list.begin(),dest_id_list.end());
    }
      
    if (mMatchType == "soft"){
      //NOTE: in this case it is enough to extract all attributes from the vertices in the ball of radius aRadius
      for (set<unsigned>::iterator it=ball.begin(); it!=ball.end();++it){
	unsigned vertex_id=*it;
	string label=GetVertexLabel(aG,vertex_id);
	InsertFeature(label,vertex_id,aG,soft_attribute_list);
      }
    } else {
      //for all vertices in ball extract the vertex's signature: distance from root-sorted distance from all the other vertices + their vertex label      
      map<unsigned,string> vertex_encoding;
      for (set<unsigned>::const_iterator it=ball.begin();it!=ball.end();++it){
	unsigned i=*it;
	vector<unsigned> encoding;
	//distance from root
	encoding.push_back(aG.PairwiseDistance(aRootVertexIndex,i));
	vector<unsigned> distance_list;
	for (set<unsigned>::const_iterator jt=ball.begin();jt!=ball.end();++jt){
	  unsigned j=*jt;
	  int dist=aG.PairwiseDistance(i,j);
	  string distance_label=stream_cast<string>(dist)+GetVertexLabel(aG,j);
	  unsigned hash_distance_label=HashFunc(distance_label,mBitMask);
	  distance_list.push_back(hash_distance_label);
	}
	sort(distance_list.begin(),distance_list.end());
	for (unsigned t=0;t<distance_list.size();t++)
	  encoding.push_back(distance_list[t]);
	unsigned hash_encoding=HashFunc(encoding,mBitMask);
	InsertFeature(stream_cast<string>(hash_encoding),i,aG,soft_attribute_list);
      }
    }
    return soft_attribute_list;  
}

//----------------------------------------------------------------------------------------------------------------------------

NSPDK3D_FeatureGenerator::NSPDK3D_FeatureGenerator(const std::string& id):ANSPDK_FeatureGenerator(id){}

void NSPDK3D_FeatureGenerator::Clear(){
  NSPDK_FeatureGenerator::Clear();
  mRootDirectionEncodingCache.clear();
}

void NSPDK3D_FeatureGenerator::GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList){
    ThirdOrderHistogramClass feature_list;
    bool empty_flag=true;
    for (unsigned i=0;i<aFirstEndpointList.size();i++){
      unsigned src_id=aFirstEndpointList[i];
      if (aG.GetVertexKernelPoint(src_id) && aG.GetVertexAlive(src_id)){
	vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(src_id,aDistance);
	for (unsigned dest_j=0;dest_j<dest_id_list.size();dest_j++){
	  unsigned dest_id=dest_id_list[dest_j];
	  if (aG.GetVertexKernelPoint(dest_id) && aG.GetVertexAlive(dest_id) ){
	    pair<unsigned,unsigned> radius_src_id=make_pair(aRadius,src_id);
	    pair<unsigned,unsigned> radius_dest_id=make_pair(aRadius,dest_id);
	    pair<unsigned,SecondOrderHistogramClass> src_feature,dest_feature;
	    vector<double> src_direction, dest_direction;
	    if (mSubgraphEncodingCache.count(radius_src_id)==0){
	      src_feature=BoundedRadiusRootedGraphCanonicalForm(src_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_src_id]=src_feature;
	      src_direction=GetRootDirection(src_id, aG, aRadius);
	      mRootDirectionEncodingCache[radius_src_id]=src_direction;
	    }
	    else {
	      src_feature=mSubgraphEncodingCache[radius_src_id];	    
	      src_direction=mRootDirectionEncodingCache[radius_src_id];
	    }
	    if (mSubgraphEncodingCache.count(radius_dest_id)==0){
	      dest_feature=BoundedRadiusRootedGraphCanonicalForm(dest_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_dest_id]=dest_feature;
	      dest_direction=GetRootDirection(dest_id, aG, aRadius);
	      mRootDirectionEncodingCache[radius_dest_id]=dest_direction;
	    }
	    else {
	      dest_feature=mSubgraphEncodingCache[radius_dest_id];
	      dest_direction=mRootDirectionEncodingCache[radius_dest_id];
	    }
	    unsigned direction_agreement=ComputeDirectionAgreement(src_direction,dest_direction);
	    vector<unsigned> endpoint_list;
	    endpoint_list.push_back(src_feature.first);
	    endpoint_list.push_back(dest_feature.first);
	    endpoint_list.push_back(direction_agreement);
	    unsigned key=HashFunc(endpoint_list,mBitMask);
	    SecondOrderHistogramClass& src_soft_feature=src_feature.second;
	    SecondOrderHistogramClass& dest_soft_feature=dest_feature.second;
	    src_soft_feature.Add(dest_soft_feature);
	    feature_list.Add(key,src_soft_feature);
	    empty_flag=false;
	  }//if dest is of active type
	}//for dest_id
      }//if src is active
    }//for src_id
    if (empty_flag){}
    else {
      vector<unsigned> distance_radius_list;
      distance_radius_list.push_back(aDistance);
      distance_radius_list.push_back(aRadius);
      unsigned distance_radius_hash=HashFunc(distance_radius_list,mBitMask);
      mFeatureList[distance_radius_hash]=feature_list;
    }
}

vector<double> NSPDK3D_FeatureGenerator::GetRootDirection(int aRootVertexIndex, const GraphClass& aG, int aRadius)const{
  vector<double> root_direction(3,0);
  if (aRadius>0){
    //extract set of vertices in the ball of radius aRadius, except the root (hence radius starts from 1)
    set<unsigned> ball; 
    for (int r=1;r<=aRadius;r++) {
      vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(aRootVertexIndex,r);
      ball.insert(dest_id_list.begin(),dest_id_list.end());
    }
    //compute average vertex-root direction
    vector<vector<double> > direction_list;
    for (set<unsigned>::const_iterator it=ball.begin();it!=ball.end();++it)
      direction_list.push_back(GetDirection(aRootVertexIndex,*it,aG));
    root_direction=ComputeAverageDirection(direction_list);
  } else {}//NOTE:return 0,0,0 as default direction for radius=0
  return root_direction;
}

double NSPDK3D_FeatureGenerator::ComputeVectorNorm(const vector<double>& aVec)const{
  double norm=0;
  for (unsigned i=0;i<aVec.size();++i)
    norm+=aVec[i]*aVec[i];
  return norm;
}

vector<double> NSPDK3D_FeatureGenerator::GetDirection(int aSrcVertexIndex, int aDestVertexIndex, const GraphClass& aG)const{
  //NOTE: the 3D coordinates are the first 3 numerical attributes of the vertices
  vector<double> direction;
  vector<double> src_dir=aG.GetVertexNumericAttributeList(aSrcVertexIndex);
  vector<double> dest_dir=aG.GetVertexNumericAttributeList(aDestVertexIndex);
  if (src_dir.size()<3) throw logic_error("Insufficient coordinate information for vertex with id:"+stream_cast<string>(aSrcVertexIndex));
  if (dest_dir.size()<3) throw logic_error("Insufficient coordinate information for vertex with id:"+stream_cast<string>(aDestVertexIndex));
  for (unsigned i=0;i<3;++i){
    direction.push_back(src_dir[i]-dest_dir[i]);
  }
  return direction;
}

unsigned NSPDK3D_FeatureGenerator::ComputeDirectionAgreement(const vector<double>& aSrcDir,const vector<double>& aDestDir)const{
  unsigned res=0;
  //compute normalized scalar product
  double src_norm=ComputeVectorNorm(aSrcDir);
  double dest_norm=ComputeVectorNorm(aDestDir);
  if (src_norm==0 && dest_norm==0) return 1;//semantic: the code value=1 is output when both norms are null
  else if ((src_norm==0 && dest_norm!=0) || (src_norm!=0 && dest_norm==0)) return 2;//semantic: the code value=2 is output when only one norm is null
  else {
    double dot_prod=0;
    for (unsigned i=0;i<aDestDir.size();++i)
      dot_prod+=aSrcDir[i]*aDestDir[i]/sqrt(src_norm*dest_norm);

    //dot product discretization
    if (dot_prod>.5) res=3;//semantic: the code value=3 is output when the two directions are parallel
    else if (dot_prod<-.5) res=4;//semantic: the code value=4 is output when the two directions are anti-parallel
    else res=5;//semantic: the code value=5 is output when the two directions are orthogonal
  }
  return res;
}

vector<double> NSPDK3D_FeatureGenerator::ComputeAverageDirection(const vector<vector<double> >& aDirectionList)const{
  if (aDirectionList.size()==0) return vector<double> (3,0); 
  else {
    const double threshold=.05;
    vector<double> direction(3,0);
    double norm;
    double min_norm=ComputeVectorNorm(aDirectionList[0]);
    for (unsigned i=0;i<aDirectionList.size();++i){
      norm=ComputeVectorNorm(aDirectionList[i]);
      if (min_norm<norm) min_norm=norm;
      for (unsigned j=0;j<3;++j){
	direction[j]+=aDirectionList[i][j];
      }
    }
    for (unsigned i=0;i<3;i++) direction[i]/=aDirectionList.size();
    norm=ComputeVectorNorm(direction);
    if (norm/min_norm<threshold) return vector<double>(3,0);//NOTE:if norm of average vector is smaller than threshold fraction of the smallest vector than output the null vector 
    else return direction;
  }
}


//----------------------------------------------------------------------------------------------------------------------------------------------
MNSPDK_FeatureGenerator::MNSPDK_FeatureGenerator(const std::string& id):NSPDK_FeatureGenerator(id){}


void MNSPDK_FeatureGenerator::Clear(){
  NSPDK_FeatureGenerator::Clear();
  mMemoizedSubgraphEncodingCache.clear();
}


void MNSPDK_FeatureGenerator::GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList){
     ThirdOrderHistogramClass feature_list;
    bool empty_flag=true;
    for (unsigned i=0;i<aFirstEndpointList.size();i++){
      unsigned src_id=aFirstEndpointList[i];
      if (aG.GetVertexKernelPoint(src_id) && aG.GetVertexAlive(src_id) ){//proceed to extract features only if the *src* vertex is a kernel point
	vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(src_id,aDistance);
	for (unsigned dest_j=0;dest_j<dest_id_list.size();dest_j++){
	  unsigned dest_id=dest_id_list[dest_j];
	  if (aG.GetVertexKernelPoint(dest_id) && aG.GetVertexAlive(dest_id)){//proceed to extract features only if the *dest* vertex is a kernel point
	    SubgraphEncodingCacheIndex radius_src_id;
	    radius_src_id.mpGraph=&aG;
	    radius_src_id.mRadius=aRadius;
	    radius_src_id.mVertexID=src_id;
	    SubgraphEncodingCacheIndex radius_dest_id;
	    radius_dest_id.mpGraph=&aG;
	    radius_dest_id.mRadius=aRadius;
	    radius_dest_id.mVertexID=dest_id;
	    pair<unsigned,SecondOrderHistogramClass> src_feature,dest_feature;
	    if (mMemoizedSubgraphEncodingCache.count(radius_src_id)==0){
	      src_feature=BoundedRadiusRootedGraphCanonicalForm(src_id, aG, aRadius);
	      mMemoizedSubgraphEncodingCache[radius_src_id]=src_feature;
	    }
	    else src_feature=mMemoizedSubgraphEncodingCache[radius_src_id];	    
	    if (mMemoizedSubgraphEncodingCache.count(radius_dest_id)==0){
	      dest_feature=BoundedRadiusRootedGraphCanonicalForm(dest_id, aG, aRadius);
	      mMemoizedSubgraphEncodingCache[radius_dest_id]=dest_feature;
	    }
	    else dest_feature=mMemoizedSubgraphEncodingCache[radius_dest_id];
	    vector<unsigned> endpoint_list;
	    if (src_feature.first<dest_feature.first) {
	      endpoint_list.push_back(src_feature.first);
	      endpoint_list.push_back(dest_feature.first);
	    } else {
	      endpoint_list.push_back(dest_feature.first);
	      endpoint_list.push_back(src_feature.first);
	    }
	    unsigned key=HashFunc(endpoint_list,mBitMask);
	    SecondOrderHistogramClass& src_soft_feature=src_feature.second;
	    SecondOrderHistogramClass& dest_soft_feature=dest_feature.second;
	    src_soft_feature.Add(dest_soft_feature);
	    feature_list.Add(key,src_soft_feature);
	    empty_flag=false;
	  }//if dest is of active type
	}//for dest_id
      }//if src is active
    }//for src_id
    if (empty_flag){}
    else {
      vector<unsigned> distance_radius_list;
      distance_radius_list.push_back(aDistance);
      distance_radius_list.push_back(aRadius);
      unsigned distance_radius_hash=HashFunc(distance_radius_list,mBitMask);
      mFeatureList[distance_radius_hash]=feature_list;
    }
}


//----------------------------------------------------------------------------------------------------------------------------------------------
RNSPDK_FeatureGenerator::RNSPDK_FeatureGenerator(const std::string& id):NSPDK_FeatureGenerator(id){}

void RNSPDK_FeatureGenerator::GenerateFeatures(const GraphClass& aG, const vector<unsigned>& aFirstEndpointList){
  //mark viewpoint vertices label
  for (unsigned i=0;i<aFirstEndpointList.size();i++){
    unsigned u=aFirstEndpointList[i];
    vector<string> symbolic_attribute_list=aG.GetVertexSymbolicAttributeList(u);
    string label="viewpoint";
    if (mMatchType == "hard"){
      for (unsigned i=0;i<symbolic_attribute_list.size();i++)
	label+="."+symbolic_attribute_list[i];
      mVertexToLabelMap[u]=label;
    } else   if (mMatchType == "hard_propertyless" || mMatchType == "soft" || mMatchType == "hard_soft" || mMatchType == "multiview" ){
      label+=aG.GetVertexLabel(u);
      mVertexToLabelMap[u]=label;
    } else {}
  }

  //generate features in the NSPDK fashion
  NSPDK_FeatureGenerator::GenerateFeatures(aG,aFirstEndpointList);

  //restore viewpoint vertices original label by erasing the cache
  for (unsigned i=0;i<aFirstEndpointList.size();i++){
    unsigned u=aFirstEndpointList[i];
    mVertexToLabelMap.erase(u);
  }
}

//----------------------------------------------------------------------------------------------------------------------------------------------
ALNSPDK_FeatureGenerator::ALNSPDK_FeatureGenerator(const std::string& id):ANSPDK_FeatureGenerator(id){}

void ALNSPDK_FeatureGenerator::GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList){
   ThirdOrderHistogramClass feature_list;
   ThirdOrderHistogramClass alias_feature_list;
    bool empty_flag=true;
    for (unsigned i=0;i<aFirstEndpointList.size();i++){
      unsigned src_id=aFirstEndpointList[i];
      if (aG.GetVertexKernelPoint(src_id) && aG.GetVertexAlive(src_id)){//proceed to extract features only if the *src* vertex is a kernel point
	vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(src_id,aDistance);
	for (unsigned dest_j=0;dest_j<dest_id_list.size();dest_j++){
	  unsigned dest_id=dest_id_list[dest_j];
	  if (aG.GetVertexKernelPoint(dest_id) && aG.GetVertexAlive(dest_id)){//proceed to extract features only if the *dest* vertex is a kernel point
	    pair<unsigned,unsigned> radius_src_id=make_pair(aRadius,src_id);
	    pair<unsigned,unsigned> radius_dest_id=make_pair(aRadius,dest_id);
	    pair<unsigned,SecondOrderHistogramClass> src_feature,dest_feature;
	    if (mSubgraphEncodingCache.count(radius_src_id)==0){
	      src_feature=BoundedRadiusRootedGraphCanonicalForm(src_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_src_id]=src_feature;
	    }
	    else src_feature=mSubgraphEncodingCache[radius_src_id];	    
	    if (mSubgraphEncodingCache.count(radius_dest_id)==0){
	      dest_feature=BoundedRadiusRootedGraphCanonicalForm(dest_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_dest_id]=dest_feature;
	    }
	    else dest_feature=mSubgraphEncodingCache[radius_dest_id];

	    //aliased features: i.e. do not consider the first endpoint
	    vector<unsigned> alias_endpoint_list;
	    alias_endpoint_list.push_back(dest_feature.first);
	    unsigned alias_key=HashFunc(alias_endpoint_list,mBitMask);
	    SecondOrderHistogramClass& alias_dest_soft_feature=dest_feature.second;
	    alias_feature_list.Add(alias_key,alias_dest_soft_feature);

	    //standard NSPDK features
	    vector<unsigned> endpoint_list;
	    endpoint_list.push_back(src_feature.first);
	    endpoint_list.push_back(dest_feature.first);
	    unsigned key=HashFunc(endpoint_list,mBitMask);
	    SecondOrderHistogramClass& src_soft_feature=src_feature.second;
	    SecondOrderHistogramClass& dest_soft_feature=dest_feature.second;
	    src_soft_feature.Add(dest_soft_feature);
	    feature_list.Add(key,src_soft_feature);
	    empty_flag=false;
	  }//if dest is of active type
	}//for dest_id
      }//if src is active
    }//for src_id
    if (empty_flag){}
    else {
      vector<unsigned> alias_distance_radius_list;
      alias_distance_radius_list.push_back(1);//use extra information to distinguish the features induced by the alias procedure
      alias_distance_radius_list.push_back(aDistance);
      alias_distance_radius_list.push_back(aRadius);
      unsigned alias_distance_radius_hash=HashFunc(alias_distance_radius_list,mBitMask);
      mFeatureList[alias_distance_radius_hash]=alias_feature_list;

      vector<unsigned> distance_radius_list;
      distance_radius_list.push_back(aDistance);
      distance_radius_list.push_back(aRadius);
      unsigned distance_radius_hash=HashFunc(distance_radius_list,mBitMask);
      mFeatureList[distance_radius_hash]=feature_list;
    }
}

//----------------------------------------------------------------------------------------------------------------------------------------------
GNSPDK_FeatureGenerator::GNSPDK_FeatureGenerator(const std::string& id):NSPDK_FeatureGenerator(id){}

void GNSPDK_FeatureGenerator::GenerateFeatures(const GraphClass& aG, int aDistance, int aRadius, const vector<unsigned>& aFirstEndpointList){
   ThirdOrderHistogramClass feature_list;
    bool empty_flag=true;
    for (unsigned i=0;i<aFirstEndpointList.size();i++){
      unsigned src_id=aFirstEndpointList[i];
      if (aG.GetVertexKernelPoint(src_id) && aG.GetVertexAlive(src_id)){//proceed to extract features only if the *src* vertex is a kernel point and is alive
	vector<unsigned> dest_id_list=aG.GetFixedDistanceVertexIDList(src_id,aDistance);
	for (unsigned dest_j=0;dest_j<dest_id_list.size();dest_j++){
	  unsigned dest_id=dest_id_list[dest_j];
	  if (aG.GetVertexKernelPoint(dest_id) && aG.GetVertexAlive(dest_id)){//proceed to extract features only if the *dest* vertex is a kernel point and is alive
	    pair<unsigned,unsigned> radius_src_id=make_pair(aRadius,src_id);
	    pair<unsigned,unsigned> radius_dest_id=make_pair(aRadius,dest_id);
	    pair<unsigned,SecondOrderHistogramClass> src_feature,dest_feature;
	    if (mSubgraphEncodingCache.count(radius_src_id)==0){
	      src_feature=BoundedRadiusRootedGraphCanonicalForm(src_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_src_id]=src_feature;
	    }
	    else src_feature=mSubgraphEncodingCache[radius_src_id];	    
	    if (mSubgraphEncodingCache.count(radius_dest_id)==0){
	      dest_feature=BoundedRadiusRootedGraphCanonicalForm(dest_id, aG, aRadius);
	      mSubgraphEncodingCache[radius_dest_id]=dest_feature;
	    }
	    else dest_feature=mSubgraphEncodingCache[radius_dest_id];
	    vector<unsigned> endpoint_list;
	    if (src_feature.first<dest_feature.first) {
	      endpoint_list.push_back(src_feature.first);
	      endpoint_list.push_back(dest_feature.first);
	    } else {
	      endpoint_list.push_back(dest_feature.first);
	      endpoint_list.push_back(src_feature.first);
	    }
	    unsigned key=HashFunc(endpoint_list,mBitMask);
	    SecondOrderHistogramClass& src_soft_feature=src_feature.second;
	    SecondOrderHistogramClass& dest_soft_feature=dest_feature.second;
	    src_soft_feature.Add(dest_soft_feature);
	    feature_list.Add(key,src_soft_feature);
	    empty_flag=false;
	    if (mDebugVerbosity>0) {
	      if (src_feature.first<dest_feature.first) 
		mDebugInfo.mHashToPlainSubgraphPairMap[key]=make_pair(src_feature.first,dest_feature.first);
	      else
		mDebugInfo.mHashToPlainSubgraphPairMap[key]=make_pair(dest_feature.first,src_feature.first);
	    }
	  }//if dest is of active type
	}//for dest_id
      }//if src is active
    }//for src_id
    if (empty_flag){}
    else {
      vector<unsigned> param_list;
      param_list.push_back(aRadius);
      unsigned param_hash=HashFunc(param_list,mBitMask);
      mFeatureList[param_hash]=feature_list;
      if (mDebugVerbosity>0) mDebugInfo.mHashToRadiusDistanceMap[param_hash]="d:?? r:"+stream_cast<string>(aRadius);
    }
}

