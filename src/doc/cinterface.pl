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

%% set_c_klog_flag(+Client:atom,+Flag:atom,+Value:atom) is det
%
% Sets C-level kLog Flag to Value for Client

%% get_c_klog_flag(+Client,+Flag,-Value) is det
%
% Unifies Value with the current value of Flag in Client.

%% document_klog_c_flags(+Client:atom,-Documentation:atom) is det
%
% Retrieve Documentation for all flags belonging to Client.

%% klog_c_flag_client(?Client) is nondet
%
% Backtrack over the IDs of the C language flag clients.

%% klog_c_flag_client_alt(?Client) is nondet
%
% Backtrack over the IDs of the C language flag clients.
% This implementation uses less memory, it doesn't store a copy of all alternatives in memory

%% c_shasum(+Filename,-HashValue) is det
%
% Unifies HashValue with the SHA1 hash value of the file (which must exist).

%% add_graph(+Id) is det
%
% Create a new graph identified by Id. Id can be any valid Prolog
% atom.

%% add_graph_if_not_exists(+Id) is det
%
% Create a new graph identified by Id unless it already exists. Id
% can be any valid Prolog atom

%% delete_graph(+Id) is det
%
% Delete graph identified by Id.

%% write_graph(+Id) is det
%
% Write graph identified by Id on the standard output.

%% export_graph(+Id,+Filename,+Format)
%
% Save graph identified by Id into Filename in specified
% Format. File extension is automatically created from format (it is
% actually identical). Valid formats are dot, csv, gml, gdl, gspan.

%% save_as_libsvm_file(+Filename) is det
%
% Save a sparse vector dataset into Filename in libsvm format. Data
% is generated for all interpretations in the attached dataset. Every
% line is a case. The line is ended by a comment giving the case
% name, which is formed by concatenating the interpretation name and
% the identifiers contained in the target fact.

%% cleanup_data is det
%
% Clean completely graphs and sparse vectors.

%% clean_internals(+GId) is det
%
% Clean up internal data structures on GId to recover memory

%% vertex_alive(+GId,+VId,?State) is det
%
% Set/get aliveness State (yes/no) of vertex VId in graph GId

%% set_signature_aliveness(+GId,+S,+State) is det
%
% Set aliveness state to State to every vertex of signature S in graph GId

%% set_slice_aliveness(+GId,+Slice,+State) is det
%
% Set aliveness state to Status to every vertex in slice Slice in graph GId

%% set_signature_in_slice_aliveness(+GId,+S,+SliceID,+State) is det
%
% Set aliveness state to State to every vertex of signature S in
% slice SliceID in graph GId

%% set_all_aliveness(+GId,+State) is det
%
% Set aliveness state to State in every vertex in graph GId

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

%% add_edge(+GId,+V,+U,+Label, -Id) is det
%
% Create new edge between U and V with label Label and unify Id with
% the identifier of the new edge

%% make_sparse_vector(+ModelType:atom,+FG:atom,+IntId:atom,+SliceId,+CaseId:atom,+ViewPoints:list_of_numbers) is det
%
% Use feature generator FG to make a sparse vector from graph Ex and
% focusing on the list of vertices in ViewPoints. The sparse vector
% is then identified by CaseId. ModelType is used to construct an
% appropriate internal representation of the feature vector.

%% write_sparse_vector(+CaseId) is det
%
% Write the sparse vector identified by CaseId.

%% add_feature_to_sparse_vector(+CaseId,+FeatureStr,+FeatureVal) is det
%
% Adds new feature to sparse vector identified by CaseId. FeatureStr
% is hashed to produce the index in the sparse vector. FeatureVal is
% then assigned to vector[hash(FeatureStr)]
% This method was introduced for ghost signatures

%% set_target_label(+CaseID,+Label) is det
%
% Set the label for SVM CaseID

%% set_label_name(+Label:atom,+NumericLabel:number) is det
%
% Record label name

%% set_rejected(+CaseID) is det
%
% Set reject flag for SVM CaseID

%% clear_rejected is det
%
% Clear all reject flags

%% remap_indices is det
%
% Remap feature vector indices in a small range

%% print_graph_ids is det
%
% Print all IDs for graphs in the dataset

%% new_feature_generator(+FGId,+FGType) is det
%
% Create a new feature generator identified by atom FGId

%% delete_feature_generator(+Id) is det
%
% Delete feature_generator identified by Id.

%% new_model(+ModelId,+ModelType) is det
%
% Create a new model identified by atom ModelId

%% delete_model(+Id) is det
%
% Delete model identified by Id.

%% model_type(+ModelId,-ModelType) is det
%
% Unifies ModelType with type of model identified by ModelId.

%% write_model(+Id) is det
%
% Write model identified by Id.

%% load_model(+Id,+Filename) is det
%
% Load model identified by Id from Filename

%% save_model(+Id,+Filename) is det
%
% Save model identified by Id into Filename in kLog internal format

%% check_model_ability(+ModelId:atom,+Ability:atom) is det
%
% Ask ModelId whether it can perform the task specified in Ability.

%% train_model(+ModelId,+DatasetSpec) is det
%
% Train model identified by ModelIdId. The list DatasetSpec should
% contain identifiers of interpretations. Case identifiers are then
% generated in Pool.h

%% test_dataset(+ModelId:atom,+DatasetSpec:atom,+NewData:atom) is det
%
% Test model identified by ModelIdId on a dataset. The list
% DatasetSpec should contain identifiers of interpretations. Case
% identifiers are then generated in Pool.h. If NewData is "true" (or
% "yes") then it is assumed that the evaluation is on test data and
% results are accumulated in the global reporter of the
% model. Otherwise it is assumed that we are testing on training data
% and only the local reporter is affected.



%% dot_product(+CaseId1,+CaseId2,-Value) is det
%
%
% Unify Value with the dot product of the feature vectors attached to
% case identifiers CaseId1 and CaseId2.


%% set_model_wd(+ModelId,+WorkingDir) is det
%
% Inform model of the working directory. This is mainly needed for
%  external models in order to keep a clean filesystem.



%% reset_reporter(+ModelId,+Reporter) is det
%
% Reset a reporter of the model. Reporter should be either local or
% global. 


%% reset_reporter(+ModelId,+Reporter,+Description) is det
%
% Reset a reporter of the model. Reporter should be either local or
% global. Also sets the description message to Description.


%% report(+ModelId,+Reporter) is det
%
% Ask a performance reporter to write a report on the
% standard output. Reporter should be either local or global.


%% report(+ModelId,+Filename,+Reporter) is det
%
% Ask a performance reporter to write a report to Filename. Reporter
% should be either local or global.


%% save_predictions(+ModelId,+Dir,+Reporter) is det
%
% Save predictions stored in a performance reporter in output.log
% and maybe output.yyy in Dir. Do AUC analysis for binary
% classification reporters. Reporter should be either local or global.


%% get_prediction(+ModelId,+CaseID,-Margin) is det
%
% Obtain prediction for CaseID in the global reporter of model
% identified by ModelId. WARNING: fails without displaying any message
% if arguments are not valid.


