/// -*- c++ -*-
/// @file   GraphClass.cc
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
#include "GraphClass.h"

std::string& getcolor(const string& s) {
    static std::map<std::string, int> colorize;
    static std::string colors[] = {"#ffaa66", "#aaff66", "#ff66aa", "#66ffaa", "#aa66ff", "#66aaff", "#ff6666", "#66ff66", "#6666ff", "#ffff66", "#ff66ff", "#66ffff", "#ffaaaa", "#aaffaa", "#aaaaff", "#ffffaa", "#ffaaff", "#aaffff", "#ffdddd", "#ddffdd", "#ddddff", "#ffffdd", "#ffddff", "#ddffff",
        "#aaaa66", "#ff9900"};
    static int global_idx = 0;
    int idx;
    std::map<std::string, int>::const_iterator found = colorize.find(s);
    if (found != colorize.end()) {
        idx = found->second;
    } else {
        idx = global_idx = ((global_idx >= 25) ? 0 : (global_idx + 1));
        colorize[s] = idx;
    }
    return colors[idx];
}

//------------------------------------------------------------------------------------------------------------------------------------------------------------

ostream& operator<<(ostream& out, const GraphClass& aG) {
    return aG.Output(out);
}

string GraphClass::GetGraphID() const {
    return mGraphID;
}

ostream& GraphClass::Output(ostream& out) const {
    for (unsigned i = 0; i < mAdjacencyList.size(); ++i) {
        out << "id: " << i << " ";
        out << "SymID: " << GetVertexSymbolicID(i) << std::endl;
        out << "  symb[ ";
        for (unsigned k = 0; k < mVertexSymbolicAttributeList[i].size(); ++k)
            out << mVertexSymbolicAttributeList[i][k] << " ";
        out << "]" << std::endl;
        out << "  num[";
        for (unsigned k = 0; k < mVertexNumericAttributeList[i].size(); ++k)
            out << mVertexNumericAttributeList[i][k] << " ";
        out << "]" << std::endl;
        out << "  status[ ";
        out << "kernel point:" << (GetVertexKernelPoint(i) ? "yes" : "no") << " ";
        out << "vertex kind:" << (GetVertexKind(i) ? "entity" : "relation") << " ";
        out << "first endpoint:" << (GetVertexViewPoint(i) ? "yes" : "no") << " ";
        out << "dead:" << (GetVertexDead(i) ? "yes" : "no") << " ";
        out << "]" << std::endl;
        out << "  Adjacency list: ";
        for (unsigned k = 0; k < mAdjacencyList[i].size(); ++k)
            out << mAdjacencyList[i][k].mDestVertexID << " ";
        out << endl;
    }

    out << "Map (src,dest) -> distance (" << mSrcDestMaptoDistance.size() << "):" << endl;
    for (map<pair<unsigned, unsigned>, int>::const_iterator it = mSrcDestMaptoDistance.begin(); it != mSrcDestMaptoDistance.end(); ++it)
        out << "(src id:" << it->first.first << " , dest id:" << it->first.second << ") -> distance:" << it->second << endl;

    out << "Map (src,distance) -> dest list (" << mSrcDistanceMaptoDestList.size() << "):" << endl;
    for (map<pair<unsigned, int>, vector<unsigned> >::const_iterator it = mSrcDistanceMaptoDestList.begin(); it != mSrcDistanceMaptoDestList.end(); ++it) {
        out << "(src id:" << it->first.first << " , distance:" << it->first.second << ") -> dest id list: ";
        const vector<unsigned>& dest_list = it->second;
        for (unsigned i = 0; i < dest_list.size(); ++i)
            out << dest_list[i] << " ";
        out << endl;
    }
    return out;
}

// void GraphClass::SaveAsUnionDotFile(const string& aGid, const string& aFilename)const{
//   std::ofstream dot_stream;
//   dot_stream.open(aFilename.c_str(),ios::out);
//   dot_stream << "graph \"" << aGid << "\"{" << std::endl;
//   for (unsigned v=0; v<mVertexSize; ++v) {
//     dot_stream << v << " [label=\"";
//     for (unsigned j=0;j<mVertexSymbolicAttributeList[v].size();++j) {
//       if (j==0)
// 	dot_stream << mVertexSymbolicAttributeList[v][j] << (mVertexSymbolicAttributeList[v].size()>1 ? "(" : "");
//       else if (j<mVertexSymbolicAttributeList[v].size()-1)
// 	dot_stream 
// 	  //<< j << ":" 
// 	  << mVertexSymbolicAttributeList[v][j] << ",";
//       else
// 	dot_stream 
// 	  //<< j << ":" 
// 	  << mVertexSymbolicAttributeList[v][j] << ")";
//     }
//     for (unsigned j=0;j<mVertexNumericAttributeList[v].size();++j) {
//       if (j==0) dot_stream << "(";
//       dot_stream 
// 	//<< j << ":" 
// 	<< mVertexNumericAttributeList[v][j]
// 	<< (j==mVertexNumericAttributeList[v].size()-1 ? ")" :",");
//     }
//     dot_stream << "\", shape=\"circle\""
// 	       << ",width=\""<< stream_cast<string>(mVertexNumericAttributeList[v][0])<<"\""
// 	       << ",color=\"" << getcolor(mVertexSymbolicAttributeList[v][0])<<"\""
// 	       << ",style=\"filled\""
// 	       <<" , penwidth=\""<<stream_cast<string>(mVertexNumericAttributeList[v][0]*3)<<"\""
//                << "]" << std::endl;
//   }
//   for (unsigned u=0; u<mVertexSize; ++u) {
//     for (unsigned vpos=0; vpos < mAdjacencyList[u].size(); ++vpos) {
//       unsigned int v = mAdjacencyList[u][vpos].mDestVertexID;
//       if (u<v){ // internally there are both u-v and v-u but we want to draw an undirected graph!
// 	unsigned edge_id = mAdjacencyList[u][vpos].mEdgeID;
// 	dot_stream << u << " -- " << v << " [label=\"";
// 	for (unsigned j=0;j<mEdgeSymbolicAttributeList[edge_id].size();++j){
// 	  dot_stream
// 	    //<< j << ":"
// 	    << mEdgeSymbolicAttributeList[edge_id][j]<<" ";
// 	}
// 	for (unsigned j=0;j<mEdgeNumericAttributeList[edge_id].size();++j){
// 	  if (j==0) dot_stream << "(";
// 	  dot_stream
// 	    //<< j << ":" 
// 	    << mEdgeNumericAttributeList[edge_id][j]
// 	    << (j==mEdgeNumericAttributeList[edge_id].size()-1 ? ")" :",");
// 	}
// 	dot_stream << "\"";
// 	dot_stream<<" , weight=\""<<mEdgeNumericAttributeList[edge_id][0]<<"\"";
// 	dot_stream<<" , penwidth=\""<<mEdgeNumericAttributeList[edge_id][0]<<"\"";
// 	dot_stream << "]" << std::endl;
//       } else {}
//     }
//   }
//   dot_stream << "}" << std::endl;
//   dot_stream.close();
// }

std::string GraphClass::vertex_label_serialize(unsigned v, std::string separator) const {
    stringstream os;
    os << (GetVertexKernelPoint(v) == true ? "*" : "");
    for (unsigned j = 0; j < mVertexSymbolicAttributeList[v].size(); ++j) {
        if (j == 0)
            os << mVertexSymbolicAttributeList[v][j] << (mVertexSymbolicAttributeList[v].size() > 1 ? "(" : "");
        else if (j < mVertexSymbolicAttributeList[v].size() - 1)
            os
                //<< j << ":"
                << mVertexSymbolicAttributeList[v][j] << ",";
        else
            os
                //<< j << ":"
                << mVertexSymbolicAttributeList[v][j] << ")";
    }
    for (unsigned j = 0; j < mVertexNumericAttributeList[v].size(); ++j) {
        if (j == 0)
            os << "[";
        os
                //<< j << ":"
                << mVertexNumericAttributeList[v][j] << (j == mVertexNumericAttributeList[v].size() - 1 ? "]" : ",");
    }
    if (GetVertexKind(v) == true) {
        std::string sym = GetVertexSymbolicID(v);
        // if (simplified_export && sym.length() > 20)
        // 	sym = sym.substr(0, 9) + ".." + sym.substr(sym.length() - 9);
        os << separator << sym;
    }
    if (!simplified_export) {
        os << separator << ">" << v << "<";
        if (IsSliced())
            os << separator << "{" << GetSliceID(v) << "}";
    }
    return os.str();
}

std::string GraphClass::edge_label_serialize(unsigned edge_id) const {
    stringstream os;
    // if (!simplified_export) 
    {
        for (unsigned j = 0; j < mEdgeSymbolicAttributeList[edge_id].size(); ++j)
            os
                //<< j << ":"
                << mEdgeSymbolicAttributeList[edge_id][j] << " ";
        for (unsigned j = 0; j < mEdgeNumericAttributeList[edge_id].size(); ++j)
            os
                //<< j << ":"
                << mEdgeNumericAttributeList[edge_id][j] << " ";
    }
    return os.str();
}

/**
 Create a dot file of a given interpretation for
 debugging/visualization. E-vertices are represented as boxes,
 R-vertices as diamonds, dead vertices are dashed. Symbolic IDs and
 slice identifiers if meaningful are printed for debugging purposes.
 */
void GraphClass::SaveAsDotFile(const string& aFilename) const {
    std::ofstream dot_stream;
    dot_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    dot_stream.open(aFilename.c_str(), ios::out);
    dot_stream << "graph \"" << mGraphID << "\"{" << std::endl;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        dot_stream << v << " [label=\"" << vertex_label_serialize(v, "\\n") << "\", shape=" << (GetVertexKind(v) == true ? "box" : "diamond") << ",style=" << (GetVertexDead(v) == false ? "filled" : "dashed") << ",fillcolor=\"" << getcolor(IsSliced() ? GetSliceID(v) : mVertexSymbolicAttributeList[v][0])
                << "\"]" << std::endl;
    }
    for (unsigned u = 0; u < mVertexSize; ++u) {
        for (unsigned vpos = 0; vpos < mAdjacencyList[u].size(); ++vpos) {
            unsigned int v = mAdjacencyList[u][vpos].mDestVertexID;
            if (v < u) { // internally there are both u-v and v-u but we want to draw an undirected graph!
                unsigned edge_id = mAdjacencyList[u][vpos].mEdgeID;
                dot_stream << u << " -- " << v << " [label=\"" << edge_label_serialize(edge_id) << "\"]" << std::endl;
            } else {
            }
        }
    }
    dot_stream << "}" << std::endl;
    dot_stream.close();
}

/**
 Create a GML file of a given interpretation for
 debugging/visualization. E-vertices are represented as boxes,
 R-vertices as diamonds, dead vertices are dashed. Symbolic IDs and
 slice identifiers if meaningful are printed for debugging purposes.
 */
void GraphClass::SaveAsGMLFile(const string& aFilename) const {
    std::ofstream gml_stream;
    gml_stream.open(aFilename.c_str(), ios::out);
    gml_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    gml_stream << "Creator \"kLog\"" << std::endl << "Version \"alpha\"" << std::endl << "graph [" << std::endl
            // << "\tcomment \"" << mGraphID << "\"" << std::endl
            << "\tdirected 0" << std::endl;
    // << "\tid 0"  << std::endl
    // << "\tlabel \"" << mGraphID << "\"" << std::endl;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        //if (GetVertexDead(v)) continue;
        gml_stream << "\tnode [" << std::endl << "\t\tid " << v << std::endl << "\t\tlabel \"" << vertex_label_serialize(v, "-") << "\"" << std::endl;
        // << "\t\tLabelGraphics [ text \""  << vertex_label_serialize(v," - ") << "\" ]" << std::endl;
        gml_stream << "\t\tgraphics [" << std::endl << "\t\t\ttype \"" << (GetVertexKind(v) == true ? "rectangle" : "diamond") << "\"" << std::endl << "\t\t\tfill \"" << (getcolor(IsSliced() ? GetSliceID(v) : mVertexSymbolicAttributeList[v][0])) << "\"" << std::endl << "\t\t\toutlineStyle \""
                << (GetVertexDead(v) == false ? "line" : "dashed") << "\"" << std::endl << "\t\t]" << std::endl;
        gml_stream << "\t]" << std::endl; // end node
    }
    for (unsigned u = 0; u < mVertexSize; ++u) {
        //if (GetVertexDead(u)) continue;
        for (unsigned vpos = 0; vpos < mAdjacencyList[u].size(); ++vpos) {
            unsigned int v = mAdjacencyList[u][vpos].mDestVertexID;
            //if (GetVertexDead(v)) continue;
            if (v < u) { // internally there are both u-v and v-u but we want to draw an undirected graph!
                unsigned edge_id = mAdjacencyList[u][vpos].mEdgeID;
                gml_stream << "\tedge [" << std::endl << "\t\tlabel \"" << edge_label_serialize(edge_id) << "\"" << std::endl;
                gml_stream << "\t\tsource " << u << std::endl << "\t\ttarget " << v << std::endl;
                gml_stream << "\t]" << std::endl; // end edge
            } else {
            }
        }
    }
    gml_stream << "]" << std::endl;
    gml_stream.close();
}

/**
 Create a GDL file of a given interpretation for
 debugging/visualization (using aiSee). E-vertices are represented
 as boxes, R-vertices as diamonds, dead vertices are
 dashed. Symbolic IDs and slice identifiers if meaningful are
 printed for debugging purposes.
 */
void GraphClass::SaveAsGDLFile(const string& aFilename) const {
    std::ofstream gdl_stream;
    gdl_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    gdl_stream.open(aFilename.c_str(), ios::out);
    gdl_stream << "graph: { title: " << "\"" << mGraphID << "\"" << std::endl << "\tlayoutalgorithm: forcedir" << std::endl << std::endl;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        gdl_stream << "node: {"
                //               << " color: \"" << (getcolor(IsSliced()?GetSliceID(v):mVertexSymbolicAttributeList[v][0]))<<"\""
                << " title: \"" << v << "\"" << " shape: " << (GetVertexKind(v) == true ? "box" : "rhomb") << " label: \"" << vertex_label_serialize(v, "\n") << "\" }" << std::endl;
    }
    for (unsigned u = 0; u < mVertexSize; ++u) {
        for (unsigned vpos = 0; vpos < mAdjacencyList[u].size(); ++vpos) {
            unsigned int v = mAdjacencyList[u][vpos].mDestVertexID;
            if (v < u) { // internally there are both u-v and v-u but we want to draw an undirected graph!
                //unsigned edge_id = mAdjacencyList[u][vpos].mEdgeID;
                gdl_stream << "edge: { " << "sourcename: \"" << u << "\"" << " targetname: \"" << v << "\" }" << std::endl;
            } else {
            }
        }
    }
    gdl_stream << "}" << std::endl;
    gdl_stream.close();
}

/**
 Create a CSV file of a given interpretation for
 debugging/visualization. Only interactions (between entities and
 relationships) are saved.
 */
void GraphClass::SaveAsCSVFile(const string& aFilename) const {
    std::ofstream csv_stream;
    csv_stream.open(aFilename.c_str(), ios::out);
    csv_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    for (unsigned u = 0; u < mVertexSize; ++u) {
        for (unsigned vpos = 0; vpos < mAdjacencyList[u].size(); ++vpos) {
            unsigned int v = mAdjacencyList[u][vpos].mDestVertexID;
            if (v < u) { // internally there are both u-v and v-u but we want to draw an undirected graph!
                unsigned edge_id = mAdjacencyList[u][vpos].mEdgeID;
                csv_stream << u << "," << vertex_label_serialize(u, " - ") << "," << v << "," << vertex_label_serialize(v, " - ") << "," << edge_id << "," << edge_label_serialize(edge_id) << std::endl;
            } else {
            }
        }
    }
    csv_stream.close();
}

void GraphClass::SaveAsGspanFile(const string& aFilename) const {
    std::ofstream gspan_stream;
    gspan_stream.open(aFilename.c_str(), ios::out);
    gspan_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    gspan_stream << "t # " << mGraphID << std::endl;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        gspan_stream << "v " << v << " " << mVertexSymbolicAttributeList[v][0] << std::endl;
    }
    for (unsigned u = 0; u < mVertexSize; ++u) {
        for (unsigned vpos = 0; vpos < mAdjacencyList[u].size(); ++vpos) {
            unsigned int v = mAdjacencyList[u][vpos].mDestVertexID;
            unsigned e = mAdjacencyList[u][vpos].mEdgeID;
            if (u < v) // internally there are both u-v and v-u but we want to draw an undirected graph!
                gspan_stream << "e " << u << " " << v << " " << mEdgeSymbolicAttributeList[e][0] << std::endl;
            else {
            }
        }
    }
    gspan_stream.close();
}

// vector<unsigned> GraphClass::GetNeighborhoodVertexIDList(unsigned aSrcID, unsigned aRadius) const {
// 	vector<unsigned> neighborhood;
// 	for (unsigned d = 0; d <= aRadius; d++) {
// 		vector<unsigned> circle = GetFixedDistanceVertexIDList(aSrcID, d);
// 		for (unsigned i = 0; i < circle.size(); ++i)
// 			neighborhood.push_back(circle[i]);
// 	}
// 	return neighborhood;
// }

/**
 Create a gSpan file given the view point list. View points are marked in their label. Only a horizon of size R+D is output
 */
void GraphClass::SaveGspanViewFile(const string& aFilename, vector<unsigned>& aViewPointList) const {
    //manage viewpoint list
    vector<unsigned> viewpoint_list;
    if (aViewPointList.size() == 0)
        for (unsigned i = 0; i < VertexSize(); i++)
            viewpoint_list.push_back(i);
    else
        viewpoint_list = aViewPointList;

    //extract all vertices within radius R+D from each viewpoint
    set<unsigned> vertex_set;
    for (unsigned i = 0; i < viewpoint_list.size(); ++i) {
        unsigned id = viewpoint_list[i];
        vector<unsigned> ball = GetNeighborhoodVertexIDList(id, mMaxRadius + mMaxDistance);
        vertex_set.insert(ball.begin(), ball.end());
    }

    //extract induced graph
    GraphClass g;
    vector<unsigned> vertex_map = GetVertexInducedSubGraph(vertex_set, g);
    g.ComputePairwiseDistanceInformation(vertex_set.size());

    //make inverse vertex map
    vector<int> inverse_vertex_map(VertexSize(), -1);
    for (unsigned i = 0; i < vertex_map.size(); i++)
        inverse_vertex_map[vertex_map[i]] = i;

    //mark vertex label of viewpoint vertices
    for (unsigned i = 0; i < viewpoint_list.size(); ++i) {
        int v_id = inverse_vertex_map[i];
        if (v_id != -1) {
            unsigned size = g.GetVertexSymbolicAttributeList(v_id).size();
            g.SetVertexSymbolicAttributeList(v_id, size, "[VP]");
        }
    }

    //save gspan file
    g.SaveAsGspanFile(aFilename);
}

void GraphClass::SaveAsMatlabFile(const string& aFilename, vector<unsigned>& aViewPointList) const {
    //manage viewpoint list
    vector<unsigned> viewpoint_list;
    if (aViewPointList.size() == 0)
        for (unsigned i = 0; i < VertexSize(); i++)
            viewpoint_list.push_back(i);
    else
        viewpoint_list = aViewPointList;

    //extract all vertices within radius R+D from each viewpoint
    set<unsigned> vertex_set;
    for (unsigned i = 0; i < viewpoint_list.size(); ++i) {
        unsigned id = viewpoint_list[i];
        vector<unsigned> ball = GetNeighborhoodVertexIDList(id, mMaxRadius + mMaxDistance);
        vertex_set.insert(ball.begin(), ball.end());
    }

    //extract induced graph
    GraphClass g;
    vector<unsigned> vertex_map = GetVertexInducedSubGraph(vertex_set, g);
    g.ComputePairwiseDistanceInformation(vertex_set.size());

    //make inverse vertex map
    vector<int> inverse_vertex_map(VertexSize(), -1);
    for (unsigned i = 0; i < vertex_map.size(); i++)
        inverse_vertex_map[vertex_map[i]] = i;

    //mark vertex label of viewpoint vertices
    for (unsigned i = 0; i < viewpoint_list.size(); ++i) {
        int v_id = inverse_vertex_map[i];
        // std::cout << "Viewpoint element " << i << " " << v_id << std::endl;
        if (v_id != -1) {
            unsigned size = g.GetVertexSymbolicAttributeList(v_id).size();
            g.SetVertexSymbolicAttributeList(v_id, size, "[VP]");
        }
    }
    // g.SaveAsGMLFile(aFilename+".gml");
    // g.SaveAsDotFile(aFilename+".dot");
    g.ExportGraph(aFilename, "-dot");
    // g.ExportGraph(aFilename,"-gml");
    // std::cout << "=========== GRAPH OUT =========" << std::endl;
    // g.Output(std::cout);
    // std::cout << "=========== END =========" << std::endl;
    return;

    //output
    string suffix = ".mat";
    //output adjacency matrix
    {
        string filename = aFilename + suffix + ".am";
        std::ofstream matlab_stream;
        matlab_stream.open(filename.c_str(), ios::out);
        matlab_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        for (unsigned i = 0; i < g.VertexSize(); ++i) {
            vector<unsigned> vertex_row(g.VertexSize(), 0);
            vector<unsigned> vertex_id_list = GetVertexAdjacentList(i);
            for (unsigned j = 0; j < vertex_id_list.size(); ++j) {
                unsigned adj_id = vertex_id_list[j];
                vertex_row[adj_id] = 1;
            }
            for (unsigned k = 0; k < g.VertexSize(); ++k) {
                matlab_stream << vertex_row[k] << " ";
            }
            matlab_stream << endl;
        }
        matlab_stream.close();
    }

    //output adjacency list
    {
        string filename = aFilename + suffix + ".al";
        std::ofstream matlab_stream;
        matlab_stream.open(filename.c_str(), ios::out);
        matlab_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        for (unsigned i = 0; i < g.VertexSize(); ++i) {
            matlab_stream << i << " ";
            vector<unsigned> vertex_id_list = GetVertexAdjacentList(i);
            for (unsigned j = 0; j < vertex_id_list.size(); ++j) {
                matlab_stream << vertex_id_list[j] << " ";
            }
            matlab_stream << endl;
        }
        matlab_stream.close();
    }

    //output vertex label
    {
        string filename = aFilename + suffix + ".nl";
        std::ofstream matlab_stream;
        matlab_stream.open(filename.c_str(), ios::out);
        matlab_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        for (unsigned i = 0; i < g.VertexSize(); i++) {
            string vertex_label = "";
            vector<string> vertex_label_list = g.GetVertexSymbolicAttributeList(i);
            for (unsigned j = 0; j < vertex_label_list.size(); ++j) {
                if (vertex_label_list[j] != "") {
                    vertex_label += vertex_label_list[j];
                    if (j < vertex_label_list.size() - 1)
                        vertex_label += "_";
                }
            }
            matlab_stream << vertex_label << endl;
        }
        matlab_stream.close();
    }

    //output edge label
    {
        string filename = aFilename + suffix + ".el";
        std::ofstream matlab_stream;
        matlab_stream.open(filename.c_str(), ios::out);
        matlab_stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
        for (unsigned i = 0; i < g.VertexSize(); ++i) {
            vector<unsigned> vertex_id_list = g.GetVertexAdjacentList(i);
            if (vertex_id_list.size() != 0) {
                vector<unsigned> edge_id_list = g.GetEdgeAdjacentList(i);
                for (unsigned j = 0; j < vertex_id_list.size(); ++j) {
                    string edge_label = "";
                    unsigned src = i;
                    unsigned dest = vertex_id_list[j];
                    unsigned edge_id = edge_id_list[j];
                    vector<string> edge_label_list = g.GetEdgeSymbolicAttributeList(edge_id);
                    for (unsigned k = 0; k < edge_label_list.size(); ++k) {
                        if (edge_label_list[k] != "") {
                            edge_label += edge_label_list[k];
                            if (k < edge_label_list.size() - 1)
                                edge_label += "_";
                        }
                    }
                    matlab_stream << src << " " << dest << " " << edge_label << endl;
                }
            }
        }
        matlab_stream.close();
    }

}

void GraphClass::ExportGraph(const string& aFilename, const string& aFormat) {
    simplified_export = false;
    std::string format;
    if (aFormat[0] == '-') {
        simplified_export = true;
        format = aFormat.substr(1);
    } else {
        format = aFormat;
    }
    try {
        if (format == "dot")
            SaveAsDotFile(aFilename + "." + format);
        else if (format == "gml")
            SaveAsGMLFile(aFilename + "." + format);
        else if (format == "gdl")
            SaveAsGDLFile(aFilename + "." + format);
        else if (format == "csv")
            SaveAsCSVFile(aFilename + "." + format);
        else if (format == "gspan")
            SaveAsGspanFile(aFilename + "." + format);
        else
            KLOG_THROW("Invalid export format >" << format << "<");
    } catch (std::ofstream::failure e) {
        KLOG_THROW("Export to " << aFilename + "." + format << " failed. Error: " << e.what());
    }
}

void GraphClass::SetSliceID(unsigned aID, const string& aSliceID) {
    if (aID >= mSliceIdList.size())
        mSliceIdList.resize(2 * mSliceIdList.size());
    mSliceIdList[aID] = aSliceID;
    mSliceIDs.insert(aSliceID);
}

string GraphClass::GetSliceID(unsigned aID) const {
    return mSliceIdList[aID];
}

void GraphClass::SetVertexKernelPoint(unsigned aID, bool aState) {
    mTopologicalChangeOccurrence = true;
    SetVertexStatusAttributeList(aID, KERNEL_POINT_ID, aState);
}

bool GraphClass::GetVertexKernelPoint(unsigned aID) const {
    return GetVertexStatusAttributeList(aID, KERNEL_POINT_ID);
}

void GraphClass::SetVertexKind(unsigned aID, bool aKind) {
    SetVertexStatusAttributeList(aID, KIND_ID, aKind);
}

bool GraphClass::GetVertexKind(unsigned aID) const {
    return GetVertexStatusAttributeList(aID, KIND_ID);
}

void GraphClass::SetVertexViewPoint(unsigned aID, bool aState) {
    SetVertexStatusAttributeList(aID, VIEWPOINT_ID, aState);
}

bool GraphClass::GetVertexViewPoint(unsigned aID) const {
    return GetVertexStatusAttributeList(aID, VIEWPOINT_ID);
}

void GraphClass::SetVertexAlive(unsigned aID, bool aState) {
    SetVertexDead(aID, !aState);
}

bool GraphClass::GetVertexAlive(unsigned aID) const {
    return !GetVertexDead(aID);
}

void GraphClass::SetVertexDead(unsigned aID, bool aState) {
    mTopologicalChangeOccurrence = true;
    SetVertexStatusAttributeList(aID, DEAD_ID, aState);
}

void GraphClass::SetSignatureAliveness(std::string aLabel, bool state) {
    mTopologicalChangeOccurrence = true;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        if (GetVertexLabel(v) == aLabel) {
            SetVertexDead(v, !state);
            //cout << "Graph_id:" << mGraphID << " killed_vertex_id:" << v <<" label:"<< vertex_label_serialize(v," - ") << std::endl;////FIXME: only for debugging purposes
        }
    }
}

void GraphClass::SetSliceAliveness(std::string slice_id, bool state) {
    mTopologicalChangeOccurrence = true;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        if (GetSliceID(v) == slice_id) {
            SetVertexDead(v, !state);
        }
    }
}

void GraphClass::SetSignatureInSliceAliveness(std::string sig, std::string slice_id, bool state) {
    mTopologicalChangeOccurrence = true;
    for (unsigned v = 0; v < mVertexSize; ++v) {
        if (GetVertexLabel(v) == sig && GetSliceID(v) == slice_id) {
            SetVertexDead(v, !state);
        }
    }
}

void GraphClass::SetAllAliveness(bool state) {
    mTopologicalChangeOccurrence = true;
    for (unsigned v = 0; v < mVertexSize; ++v)
        SetVertexDead(v, !state);
}

bool GraphClass::GetVertexDead(unsigned aID) const {
    return GetVertexStatusAttributeList(aID, DEAD_ID);
}

void GraphClass::SetVertexLabel(unsigned aID, const string& aLabel) {
    SetVertexSymbolicAttributeList(aID, LABEL_VERTEX_ATTRIBUTE_ID, aLabel);
}

string GraphClass::GetVertexLabel(unsigned aID) const {
    return GetVertexSymbolicAttributeList(aID, LABEL_VERTEX_ATTRIBUTE_ID);
}

string GraphClass::GetEdgeLabel(unsigned aSrcID, unsigned aDestID) const {
    return GetEdgeSymbolicAttributeList(aSrcID, aDestID, EDGE_ATTRIBUTE_ID);
}

void GraphClass::SetEdgeLabel(unsigned aSrcID, unsigned aDestID, const string& aLabel) {
    SetEdgeSymbolicAttributeList(aSrcID, aDestID, EDGE_ATTRIBUTE_ID, aLabel);
}

bool GraphClass::Check() const {
    if (VertexSize() == 0)
        KLOG_THROW("Empty vertex set >" << mGraphID << "<");
    if (EdgeSize() == 0)
        cerr << "Warning: Empty edge set >" << mGraphID << "<" << endl; //KLOG_THROW("Empty edge set >" << mGraphID << "<");
    if (mSrcDestMaptoDistance.size() == 0)
        KLOG_THROW("Empty (src,dest) -> distance map >" << mGraphID << "<");
    if (mSrcDistanceMaptoDestList.size() == 0)
        KLOG_THROW("Empty (src,distance) -> dest list map >" << mGraphID << "<");
    return true;
}

/**
 Returns a vector of ids of vertices adjacent to aID that are
 ALIVE. Overriden function from
 BaseGraphClass::GetVertexAdjacentList(unsigned aID)const.
 */
vector<unsigned> GraphClass::GetVertexAdjacentList(unsigned aID) const {
    vector<unsigned> adjacent_list;
    for (unsigned i = 0; i < mAdjacencyList[aID].size(); ++i) {
        unsigned vid = mAdjacencyList[aID][i].mDestVertexID;
        if (GetVertexAlive(vid))
            adjacent_list.push_back(vid);
    }
    return adjacent_list;
}

void GraphClass::ComputePairwiseDistanceInformation(int aMaxDistance, int aMaxRadius) const {
    if (aMaxDistance != -1)
        mMaxDistance = aMaxDistance;
    if (aMaxRadius != -1)
        mMaxRadius = aMaxRadius;
    if (mTopologicalChangeOccurrence == true) {
        unsigned distance_bound = max(mMaxRadius, mMaxDistance);
        mSrcDestMaptoDistance.clear();
        for (unsigned i = 0; i < VertexSize(); i++) {
            if (GetVertexDead(i) == false) // Paolo
                SingleVertexBoundedBreadthFirstVisit(i, distance_bound, mSrcDestMaptoDistance);
        }
        mSrcDistanceMaptoDestList.clear();
        for (map<pair<unsigned, unsigned>, int>::iterator it = mSrcDestMaptoDistance.begin(); it != mSrcDestMaptoDistance.end(); ++it) {
            unsigned src_id = it->first.first;
            unsigned dest_id = it->first.second;
            int distance = it->second;
            mSrcDistanceMaptoDestList[make_pair(src_id, distance)].push_back(dest_id);
        }

        mTopologicalChangeOccurrence = false;
    } else {
    }
}

void GraphClass::clean_internals(void) {
    mSrcDestMaptoDistance.clear();
    mSrcDistanceMaptoDestList.clear();
    mTopologicalChangeOccurrence = true;
}

void GraphClass::SingleVertexBoundedBreadthFirstVisit(unsigned aRootVertexIndex, int aRadius, map<pair<unsigned, unsigned>, int>& oSrcDestMaptoDistance) const {
    map<int, int> dest_mapto_distance;
    dest_mapto_distance[aRootVertexIndex] = 0;
    map<int, bool> already_explored;
    already_explored[aRootVertexIndex] = true;
    queue<int> q;
    q.push(aRootVertexIndex); //initialize queue with the root vertex
    while (q.empty() == false) {
        int u = q.front();
        for (unsigned j = 0; j < mAdjacencyList[u].size(); j++) {
            int v = mAdjacencyList[u][j].mDestVertexID;
            if (already_explored[v] == true) {
            } else if (GetVertexDead(v) == true) {
            }                //add check of dead edge
            else {
                if (dest_mapto_distance[u] + 1 <= aRadius) {
                    dest_mapto_distance[v] = dest_mapto_distance[u] + 1;
                    already_explored[v] = true;
                    q.push(v);
                }
            }
        }
        q.pop();
    }
    //compute (src,dest) \mapto distance
    for (map<int, int>::const_iterator it = dest_mapto_distance.begin(); it != dest_mapto_distance.end(); ++it) {
        unsigned dest_vertex_id = (unsigned) (it->first);
        int distance = it->second;
        oSrcDestMaptoDistance.insert(make_pair(make_pair(aRootVertexIndex, dest_vertex_id), distance));
    }
}

vector<unsigned> GraphClass::GetFixedDistanceVertexIDList(unsigned aSrcID, int aDistance) const {
    if (mTopologicalChangeOccurrence == true)
        ComputePairwiseDistanceInformation();
    if (mSrcDistanceMaptoDestList.count(make_pair(aSrcID, aDistance)) == 0)
        return vector<unsigned>(0);
    else
        return mSrcDistanceMaptoDestList.find(make_pair(aSrcID, aDistance))->second;
}

int GraphClass::PairwiseDistance(unsigned aSrcID, unsigned aDestID) const {
    if (mTopologicalChangeOccurrence == true)
        ComputePairwiseDistanceInformation();
    if (mSrcDestMaptoDistance.count(make_pair(aSrcID, aDestID)) == 0)
        return -1;
    else
        return (mSrcDestMaptoDistance.find(make_pair(aSrcID, aDestID)))->second;
}

vector<unsigned> GraphClass::GetNeighborhoodVertexIDList(unsigned aSrcID, unsigned aRadius) const {
    if (mTopologicalChangeOccurrence == true)
        ComputePairwiseDistanceInformation();
    vector<unsigned> neighborhood;
    for (unsigned d = 0; d <= aRadius; d++) {
        vector<unsigned> circle = GetFixedDistanceVertexIDList(aSrcID, d);
        for (unsigned i = 0; i < circle.size(); ++i)
            neighborhood.push_back(circle[i]);
    }
    return neighborhood;
}
