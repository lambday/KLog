/*
Learning procedures

Copyright (C) 2013-  Fabrizio Costa, Kurt De Grave, Luc De Raedt, Paolo Frasconi

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(learn, [train/4,
                  predict/4,
                  generate_all/3,
                  induce_all/2,
                  declare_atomic_labels/1,
                  save_induced_facts/2
		 ]).

/** <module> Learning support in kLog

  Formulate kLog learning jobs. Below are some examples of target
  relations that kLog may be asked to learn. Currently, implemented
  models have abilities to cover only a fraction of these.
  
  ---++ Jobs with relational arity = 0:

  ---+++ Binary classification of each interpretation.

  *Example*: classification of small molecules.
  ==
  signature signature mutagenic::extensional.
  ==
  
  ---+++ Regression for each interpretation.
  
  *Example*: predict the affinity of small molecules binding.
  ==
  signature affinity(strength::property)::extensional.
  ==

  ---+++ Multitask on individual interpretations.
  
  *Example*: predict mutagenicity level and logP for small molecules.
  ==
  signature
    molecule_properties(mutagenicity::property(real),
                       logp::property(real))::extensional.
  ==
  
  ---+++ Multiclass classification for each interpretation
  
  *Example*: image categorization;
  ==
  signature image_category(cat::property)::extensional.
  ==
  
  ---++ Jobs with relational arity = 1:

  ---+++ Binary classification of entities in each interpretation

  *Examples*: detect spam webpages as in the Web Spam Challenge
   (http://webspam.lip6.fr/wiki/pmwiki.php), predict blockbuster
   movies in IMDb
  
  ==
  signature spam(url::page)::extensional.
  signature blockbuster(m::movie)::extensional.
  ==
  
  ---+++ Multiclass classification of entities in each interpretation

  *Examples*: WebKB, POS-Tagging, NER, protein secondary structure prediction

  ==
  signature page(url::page,category::property)::extensional.
  signature pos_tag(word::position,tag::property)::extensional. 
  signature named_entity(word::position,ne::property)::extensional.
  signature secondary_structure(r::residue,ss::property)::extensional.
  ==

  ---+++ Multiclass classification of entities in each interpretation

  *Example*: traffic flow forecasting

  ==
  signature flow_value(s::station,flow::property(real))::extensional.
  ==
  
  ---++ Jobs with relational arity = 2:

  ---+++ Link prediction tasks

  *Examples*: protein beta partners, UW-CSE, Entity resolution,
   Protein-protein interactions

  ==
  signature partners(r1::residue,r2::residue)::extensional.
  signature advised_by(p1::person,p2::person)::extensional.
  signature same_venue(v1::venue,v2::venue)::extensional.
  signature phosphorylates(p1::kinase,p2::protein)::extensional.
  signature regulates(g1::gene,g2::gene)::extensional.
  ==
  
  ---+++ Regression on pairs of entities

  *Example*: traffic flow forecasting at different stations and
   different lead times, Prediction of distance between protein
   secondary structure elements

  ==
  signature congestion(s::station,lead::time,
                       flow::property(float))::extensional.
  signature distance(sse1:sse,sse2:sse,d::property(float))::intensional.
  ==
  
  ---+++ Pairwise hierarchical classification
  
  *Example*: traffic congestion level forecasting at different stations and different lead times

  ==
  signature congestion_level(s::station,lead::time,
                             level::property)::extensional.
  ==
  
  ---++ Tasks with relational arity > 2:
  
  ---+++ Prediction of hyperedges
  
  *Example*: Spatial role labeling
  
  ==
  signature ttarget(w1::sp_ind_can, w2::trajector_can,
                    w3::landmark_can)::intensional.
  ==
  *Example*: Metal binding geometry
  
  ==
  signature binding_site(r1::residue,r2::residue,
                         r3::residue,r4::residue)::extensional.
  ==
  
  ---+++ Classification of hyperedges
  
  *Example*: Metal binding geometry with ligand prediction
  
  ==
  signature binding_site(r1::residue,r2::residue,
                         r3::residue,r4::residue,
                         metal::property)::extensional.
  ==

  ---++ Subsampling
  

  The following predefined dynamic predicate fails by default:
  
  ==
    user:klog_reject(+Case,+TrainOrPredict)
  ==
  
  By defining your own version of it, it is easy to implement
  selective subsampling of cases. A typical usage would be for dealing
  with a highly imbalanced data set where you want to subsample only a
  fraction of negatives. Case is a Prolog callable predicate
  associated with a training or testing case. When kLog generates
  cases, it calls user:klog_reject/2 to determine if the case should
  be rejected. TrainOrPredict should be either 'train' or 'predict' to
  do subsampling either at training or testing time. For example
  suppose we have a binary classification task and we want to reject
  90 percent of the negatives at training time. Then the following
  code should be added to the kLog script.

  ==
  klog_reject(Case,train) :-
    \+ call(Case),
    random(R),
    R>0.1.
  ==

  ---++ Predicates

  
  @author Paolo Frasconi

*/

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(random)).
:- ensure_loaded('graphicalize').
:- ensure_loaded('timing').
:- ensure_loaded('utils').
:- ensure_loaded('C/c_interface').
:- ensure_loaded('syntax').
:- ensure_loaded('reachability').
:- ensure_loaded('flags').


:- dynamic user:klog_reject/2. % placate undefined predicate warning

:- dynamic user:safe/1.

:- table depends_transitive/2.
%% depends_transitive(?S1:atom,?S:atom) is det
%
% Tabled. Contains dependency analysis results to properly kill vertices in
% the graphs. The predicate succeeds if S1 ``depends'' on S.  The
% mechanism actually goes a bit beyond a simple transitive closure of
% the call graph: when several targets are present in the same file it
% is necessary to extend the definition of dependencies. The set of
% dependent signatures is defined as follows:
%
% 1. All ancestors, including S itself (obvious)
%
% 2. All the descendants (less obvious; however, if a target signature
% S is intensional and calls some predicate Y in its definition, then
% Y supposedly (although not necessarily) contains some supervision
% information, which should be removed).
%
% 3. All the ancestors of the descendants (maybe even less obvious,
% but if now there is some other signature T which calls Y, then T
% will also contain supervision information).
%
% The descendants of the ancestors need not to be removed.
%
% Some of the ancestors of S might be legitimate predicates that have
% nothing to do with supervision. Therefore we restrict ourselves to
% the subset of the call graph with predicates that are either in the
% user: namespace (where the 'user' could cheat accessing supervision
% information) or in the data set file (in the db: namespace). In any
% case, since removal of vertices from the graph might surprise the
% user, kLog issues a warning if there are killed signatures besides
% the target signature of the current training or test
% procedure. Finally, one can declare a signature to be safe using
% safe/1. In this case it is assumed that the Prolog code inside it
% does not exploit any supervision information. safe/1 should be used
% with care.
%
% The table depends on current_depends_directly/2 which is asserted by
% record_dependencies/0, called at the end of graphicalize:attach/1.


depends_transitive(Query,Query).
depends_transitive(P,Query) :-
    current_depends_directly(P,Query),
    \+ user:safe(P),
    %% writeln(depends_transitive1(P,Query)),
    true.
depends_transitive(P,Query) :-
    current_depends_directly(P,P1),
    depends_transitive(P1,Query),
    \+ user:safe(P),
    %% writeln(depends_transitive2(P,Query)),
    true.
depends_transitive(P,Query) :-
    current_depends_directly(Query,P),
    \+ user:safe(P),
    %% writeln(depends_transitive3(P,Query)),
    true.
depends_transitive(P,Query) :-
    current_depends_directly(Query,P1),
    depends_transitive(P1,P),
    \+ user:safe(P),
    %% writeln(depends_transitive4(P,Query)),
    true.

%% :- table iscurrent/1.
iscurrent(X) :- X\=':'/2, X\='.'/2, current_predicate(user:X).
iscurrent(X) :- X\=':'/2, X\='.'/2, X=P/N, length(Args,N),  F=..[P|Args], once(db:interpretation(_,F)).

:- dynamic current_depends_directly/2.
record_dependencies :-
    forall(depends_directly(A,B),
           ( iscurrent(A),iscurrent(B) ->
             assert(current_depends_directly(A,B)) ; true )),
    save_dependecies.

save_dependecies :-
    save_direct_dependencies,
    klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
    ( V @>= 4 -> save_direct_dependencies ; true).
save_direct_dependencies :-
    open('dependencies.dot',write,Stream),
    format(Stream,'digraph dependency {~nnode [shape=box]~n',[]),
    forall(current_depends_directly(A,B),
           format(Stream,'"~w" -> "~w"~n',[A,B])),
    format(Stream,'}~n',[]),
    close(Stream).
save_transitive_dependencies :-
    setof(A,B^C^(current_depends_directly(A,B);current_depends_directly(C,A)),PredsOfInterest),
    forall( member(P,PredsOfInterest),
            ( P=Name/Arity,
              atomic_list_concat([Name,'-',Arity,'-dep.dot'],Filename),
              open(Filename,write,Stream),
              format(Stream,'digraph "~w" {~nnode [shape=box]~n',[P]),
              forall(depends_transitive(A,P),
                     format(Stream,'"~w" -> "~w"~n',[A,P])),
              format(Stream,'}~n',[]),
              close(Stream)
            )).


check_kernel_point_vs_target(S/A) :-
        is_kernel_point(S,yes),
        atomic_list_concat(['signature >',S,'/',A,'< is a kernel point and a target - cannot train'],Message),
        throw(error(klog_error, (check_kernel_point_vs_target(S/A), Message))).
check_kernel_point_vs_target(S/A) :-
        depends_transitive(S1/A1,S/A),
        is_kernel_point(S1,yes),
        atomic_list_concat(['signature >',S1,'/',A1,'< is a kernel point that depends on the target >',S,'/',A,'< - cannot train'],Message),
        throw(error(klog_error, (check_kernel_point_vs_target(S/A), Message))).
check_kernel_point_vs_target(_/_).


%% train(+S:atom,+Examples:list_of_atoms,+Model:atom,+Feature_Generator:atom) is det
%
% Train Model on a data set of interpretations, using
% Feature_Generator as the feature generator. Examples is a list of
% interpretation identifiers. S is a signature. For each possible
% combination of identifiers in S, a set of "cases" is generated where
% a case is just a pseudo-iid example. If S has no properties,
% positive cases are those for which a tuple exist in the
% interpretation and negative cases all the rest. If S has one
% property, this property acts as a label for the case (can be also
% regression if the property is a real number). Two or more properties
% define a multi-task problem (currently handled as a set of
% independent tasks). Problems like small molecules classification
% have a target signature with zero arity which works fine as a
% special case of relationship. Model should have the ability of
% solving the task specified in the target signature S.
%
% @error if the target signature is a kernel point (training on it would be cheating)
%
% @error if Model cannot solve the task specified by S.
%
% @tbd Multitask taking correltations into account

% Only separate tasks for now, multitask will be different...
% Multiclass can be done in principle with this code (model will
% receive an integer label). However it's probably better to work on
% multiclass together with structured output (when it will be
% implemented)
train(TargetSignature,Examples,Models,Feature_Generator) :-
    klog_writeln(4,train(TargetSignature,Examples,Models,Feature_Generator)),
    flatten(Models,ListOfModels), % so that either model_1 or [model_1,model_2] will be equally valid
    %% First do a check for all tasks
    check_job(TargetSignature,ListOfModels),
    %% then proceed training the models
    kill_present(TargetSignature,Examples),
    kill_future(Examples),
    debug_graphs(Examples,train),
    forall( get_task(TargetSignature,TaskIndex,TaskName,TaskType),
            (klog_writeln(4,task(TargetSignature,TaskIndex,TaskName,TaskType)),
             nth0(TaskIndex,ListOfModels,Model),
             cases_loop(TargetSignature,Examples,Model,Feature_Generator,TaskName,TaskType,train)
            )
          ),
    resuscitate_present_and_future(Examples),
    true.

check_job(TargetSignature,ListOfModels) :-
    ( once(signature_traits(TargetSignature,_,_)) ->
      true
    ;
      throw(error(klog_error, (train((TargetSignature)), 'Target signature does not exist')))
    ),
    forall( get_task(TargetSignature,TaskIndex,_TaskName,TaskType),
            ( nth0(TaskIndex,ListOfModels,Model),
              %% writeln(check_model_ability(Model,TaskType)),
              check_model_ability(Model,TaskType)
            )
          ).

:- use_module(library(ordsets)).

%% kill_present(+TargetSignature:atom,+Examples:list) is det
%
% Predicates kill_present/2 and kill_future/1 are needed to setup training and
% test data. Let's
% call vertices associated with the target signature plus all their
% dependents (defined by depends_transitive/2) the set of "query"
% vertices. In the case of unsliced interpretations, all query
% vertices must be killed. The case of sliced interpretations is more
% tricky and is handled as follows:
%
% Example using IMdB, slice_preceq defined by time (years)
%
% Suppose data set contains imdb(1953),...,imdb(1997) and that we want
% to train on [imdb(1992),imdb(1993)] and test on
% [imdb(1995),imdb(1996)]. Then during training we first take the most
% recent year in the training set (1993) and kill *everything*
% strictly in the future (i.e. 1994, 1995, 1996, 1997) using
% kill_future/1 plus the query vertices in the present (i.e. 1992 and
% 1993) using kill_present/2. Test is similar, we take the most recent
% year in the test set (1996) and kill *everything* strictly in the
% future (i.e. 1997) plus the query vertices in the present (i.e. 1995
% and 1996). Thus, for example, movies of 1994 are not used for
% training but during prediction their labels are (rightfully)
% accessible for computing feature vectors. In a hypothetical
% transductive setting we might keep alive vertices in the future for
% "evidence" signatures. However this is not currently supported.
%
% kill_present/2 kills vertices associated with the TargetSignature
% (and dependents) in the present slices. If interpretations are not
% sliced vertices are killed anyway.
kill_present(TargetSignature,Examples) :-
    signature_traits(TargetSignature,arity,M),
    setof((S1/N), depends_transitive(S1/N,TargetSignature/M),Set),
    length(Set,NKilled),
    ( NKilled>1 -> klog_warning(vertices_removed_from_graph(Set)) ; true ),
    forall( member(Ex,Examples),
            ( ( Ex=..[IntId|[SliceId]] -> % Sliced
                atomic_list_concat([SliceId],ASliceId), % Just in case, slice_id may be an integer
                forall( depends_transitive(S1/_,TargetSignature/_),
                        (% kdebug(killinslice(IntId,S1,ASliceId)),
                          kill_signature_in_slice(IntId,S1,ASliceId)
                        )
                      )
              ;
                IntId=Ex,
                forall( depends_transitive(S1/_,TargetSignature/M),
                        (% kdebug(kill(IntId,S1)),
                          kill_signature(IntId,S1)
                        )
                      )
              )
            )
          ).

%% kill_future(+Examples:list) is det
%
% Kill entire slices in the strict future (if there are no sliced
% interpretations kill_future/1 does nothing since max_slice will
% fail)
kill_future(Examples) :-
    setof(S,Ex^Arg^(member(Ex,Examples),Ex=..[S|Arg]),IntIds),
    ( max_slice(Examples,Max) ->
      forall( member(IntId,IntIds),
              ( forall( (db:slice_preceq(Max,Future),\+Max=Future),
                        ( %kdebug(killing_slice(IntId,Future)),
                          atomic_list_concat([Future],AFuture), % Just in case, slice_id may be an integer
                          kill_slice(IntId,AFuture),
                          true
                        )
                      )
              )
            )
    ;
      true
    ).
    
% After training, slices in the present and in the future need to be
% resuscitated. For non-sliced interpretations this predicate does
% nothing.
resuscitate_present_and_future(Examples) :-
    setof(S,Ex^Arg^(member(Ex,Examples),Ex=..[S|Arg]),IntIds),
    ( min_slice(Examples,Min) ->
      forall( member(IntId,IntIds),
              ( forall( db:slice_preceq(Min,Future),
                        ( %kdebug(resuscitating_slice(IntId,Future)),
                          atomic_list_concat([Future],AFuture), % Just in case, slice_id may be an integer
                          resuscitate_slice(IntId,AFuture),
                          true
                        )
                      )
              )
            )
    ;
      true
    ).

max_slice(Examples,Max) :-
    list_of_slices(Examples,ExList),
    preceq_max_list(ExList,Max).
min_slice(Examples,Max) :-
    list_of_slices(Examples,ExList),
    preceq_min_list(ExList,Max).


%% list_of_slices(+SlicedInterpretations,?Slices) is det
%
% Slices is unified with the list of slices found in SlicedInterpretations.
list_of_slices([],[]).
list_of_slices([Ex|Rest],[SliceId|Tail]) :-
    Ex=..[_IntId,SliceId],
    list_of_slices(Rest,Tail).
list_of_slices([Ex|Rest],Tail) :-
    atomic(Ex),
    list_of_slices(Rest,Tail).

%% preceq_max_list(+List,?M) is det
%
% M is unified with the max element in List according to the total
% order defined by db:slice_preceq/2.
preceq_max_list([MaxSoFar|Tail],Max) :- preceq_max_list(Tail,MaxSoFar,Max).
preceq_max_list([],Max,Max).
preceq_max_list([Head|Tail],MaxSoFar,Max) :-
    preceq_max(Head,MaxSoFar,MaxSoFar1),
    preceq_max_list(Tail,MaxSoFar1,Max).
preceq_max(A,B,Max) :-
    ( db:slice_preceq(B,A) ->
      Max = A
    ;
      ( db:slice_preceq(A,B) ->
        Max = B
      ;
        throw(error(klog_error, (preceq_max(A,B), ' weird.. args out of domain')))
      )
    ).

%% preceq_min_list(+List,?M) is det
%
% M is unified with the min element in List according to the total
% order defined by db:slice_preceq/2.
preceq_min_list([MinSoFar|Tail],Min) :- preceq_min_list(Tail,MinSoFar,Min).
preceq_min_list([],Min,Min).
preceq_min_list([Head|Tail],MinSoFar,Min) :-
    preceq_min(Head,MinSoFar,MinSoFar1),
    preceq_min_list(Tail,MinSoFar1,Min).
preceq_min(A,B,Min) :-
    ( db:slice_preceq(B,A) ->
      Min = B
    ;
      ( db:slice_preceq(A,B) ->
        Min = A
      ;
        throw(error(klog_error, (preceq_min(A,B), ' weird.. args out of domain')))
      )
    ).

%% predict(+S:atom,+Examples:list_of_atoms,+Model:atom,+Feature_Generator:atom) is det
%
% Test Model on a data set of interpretations. Use Feature_Generator as the feature
% generator. Examples is a list of interpretation identifiers. S is a
% signature. Cases are generated as in train/3. The predicate asserts
% induced facts as follows:
%
% ==
% induced(InterpretationId,db:interpretation(InterpretationID,Fact)).
% ==
predict(TargetSignature,Examples,Models,Feature_Generator) :-
    klog_writeln(4,predict(TargetSignature,Examples,Models,Feature_Generator)),
    flatten(Models,ListOfModels), % so that either model_1 or [model_1,model_2] will be equally valid
    %% First do a check for all tasks
    check_job(TargetSignature,ListOfModels),
    %% then proceed using the models for prediction
    kill_present(TargetSignature,Examples),
    kill_future(Examples),
    debug_graphs(Examples,predict),
    forall( get_task(TargetSignature,TaskIndex,TaskName,TaskType),
            ( nth0(TaskIndex,ListOfModels,Model),
              cases_loop(TargetSignature,Examples,Model,Feature_Generator,TaskName,TaskType,predict)
            )
          ),
    resuscitate_present_and_future(Examples),
    true.

% dump a few graphs for debugging
debug_graphs(Examples,When) :-
    setof(S,Ex^Arg^(member(Ex,Examples),Ex=..[S|Arg]),IntIds),
    get_klog_flag(n_debug_graphs,NDebug),
    forall( (nth0(N,IntIds,IntId),N<NDebug), % limit to the first NDebug interpretations
            ( atomic_list_concat([When,'-',IntId],Filename),
              export_graph(IntId,Filename,dot)
            )
          ).

%% get_task(+TargetSignature:atom,-TaskIndex:integer,-TaskName:atom,-Values:list) is nondet
%
% Given TargetSignature, retrieve the i-th task (starting from 0),
% unify TaskIndex with i, TaskName with its name and Values with the
% list of target values found in the data set for this task. TaskName
% is either in the form N#P where N is the property name and P the
% position in the argument list, or the atom 'callme' meaning that the
% task is to learn a relationship.
get_task(TargetSignature,_,_,_) :-
        atomic_list_concat([TargetSignature,'_',task_index],ValName),
        nb_setval(ValName,0), fail.
get_task(TargetSignature,TaskIndex,Name#N,TaskType) :-
        signature_traits(TargetSignature,arity,A),
        length(Args,A),
        signature_traits(TargetSignature,column_types,Types),
        signature_traits(TargetSignature,column_names,Names),
        nth0(N,Types,property),
        nth0(N,Names,Name),
        nth0(N,Args,Arg,Rest),
        Goal=..[TargetSignature|Args],
        setof(Arg,Ex^Rest^(db:interpretation(Ex,Goal)),Values),
        task_type(Values,TaskType),
        atomic_list_concat([TargetSignature,'_',task_index],ValName),
        nb_getval(ValName,TaskIndex),
        I1 is TaskIndex+1,
        nb_setval(ValName,I1).
% special case: relationship
get_task(TargetSignature,TaskIndex,callme,binary_classification) :-
        signature_traits(TargetSignature,relational_arity,R),
        signature_traits(TargetSignature,arity,R),
        atomic_list_concat([TargetSignature,'_',task_index],ValName),
        nb_getval(ValName,TaskIndex).


task_type([false,true],binary_classification) :- !.
task_type(Values,binary_classification) :-
        forall(member(Val,Values), atom(Val)),
        length(Values,2),!.
task_type([-1,1],binary_classification) :- !.
task_type(Values,multiclass_classification) :-
        forall(member(Val,Values), atom(Val)),
        length(Values,K), K>2,!.
task_type(Values,regression) :-
        forall(member(Val,Values), number(Val)),!.
task_type(_,nil) :-
        throw(error(klog_error, (task_type/2, 'Cannot determine task type, most likely numerical and categorical properties are mixed in the data'))).

:- dynamic(induced/2).
:- dynamic(margin/6).

sforall(A,B) :- forall(A,B).
sforall(_,_).

feature_to_float(V,V) :-
        float(V).
feature_to_float(V,F) :-
        integer(V),
        F is V + 0.0.
feature_to_float(V,F) :-
        atomic_list_concat(['Feature value ',V,' cannot be immediately converted to a float'],Message),
        throw(error(klog_error, (feature_to_float(V,F), Message))).

%% cases_loop(+TS,+Examples,+Model,+FG,+TName,+TType,+TOrP) is det
%
% Core procedure for training (if TOrP=train) or testing (if
% TOrP=predict). TS is the target signature. Examples is a list of
% interpretations (from which cases are built). Model is a kLog
% model. FG is a kLog feature generator. TName and TType are the task
% name and type as returned by get_task/4. The loop generates all
% cases for each interpretation (using tuple_of_identifiers/3) and
% creates (if necessary) all required feature vectors and output
% labels. In prediction mode, cases are immediately predicted. In
% training mode, C++-level predicates train_model/2 and test_dataset/3
% are called at the end of the loop. In both cases, results are
% accumulated both in the local and the global reporters. However the
% local reporter is reset when this loop starts. This is useful for
% obtaining training set accuracy and test set accuracy of individual
% folds in k-fold-CV.
cases_loop(TargetSignature,Examples,Model,Feature_Generator,TaskName,TaskType,TrainOrPredict) :-
        bagof(IDTuple, Ex^(member(Ex,Examples),tuple_of_identifiers(Ex,TargetSignature,IDTuple)),Cases),
        length(Cases,NCases),
        klog_writeln(4,number_of_cases(NCases)),
        new_progress_bar(cases_pb,NCases),
        klog_writeln(4,cases_loop(TargetSignature,Examples,Model,Feature_Generator,TaskName,TaskType)),
        (TrainOrPredict=predict -> reset_reporter(Model,local,'Test set') ; true),
        clear_rejected,
	forall( member(Ex,Examples),
		( forall( tuple_of_identifiers(Ex,TargetSignature,IDTuple),
                          once(( klog_writeln(5,tuple_of_identifiers(Ex,TargetSignature,IDTuple)),
                            deal_with_case(Ex,TargetSignature,Model,Feature_Generator,TaskName,IDTuple,TrainOrPredict),
                            progress_bar(cases_pb)
			    ; true
                          ))),
                  prolog_clean_internals(Ex)
                )),
        klog_color_format(3,'~nCreated feature vectors~n',[],green),
        sforall(atomic_label(Y,Yint),klog_color_format(4,'Label ~w --> ~w~n',[Y,Yint],cyan)),
        klog_format(3,'~n',[]),
        atomize_interpretation_list(Examples,AExamples),
        ( TrainOrPredict=train ->
          use_timer(model_train,train_model(Model,AExamples)),
          use_timer(model_test,test_dataset(Model,AExamples,false)) % test on training set
        ;
          ( TrainOrPredict=predict ->
            use_timer(model_test,test_dataset(Model,AExamples,true))
            ; true
          ),
          true
        ).


deal_with_case(Ex,TargetSignature,Model,Feature_Generator,TaskName,IDTuple,TrainOrPredict) :-
    make_case(Ex,TargetSignature,IDTuple,Case),
    make_case_id(Ex,TargetSignature,IDTuple,CaseID),
    %% writeln(make_case(Ex,TargetSignature,IDTuple,Case)),
    get_case_label(Case,TaskName,Y),
    %% writeln(get_case_label(Case,TaskName,Y)),
    map_prolog_ids_to_vertex_ids(Ex,IDTuple,ViewPoint),
    use_timer(sparse_vectors,
              prolog_make_sparse_vector(Model,Feature_Generator,Ex,CaseID,ViewPoint)),
    %% FIXME: Raise exception if ghost with no properties (NP==0)
    %% FIXME: This code requires identifiers first and properties after...
    sforall( signature_traits(G,ghost,yes),
             (
              %% writeln(('=====',G,'======')),
              signature_traits(G,arity,GArity),
              signature_traits(G,relational_arity,GRArity),
              NP is GArity - GRArity,
              length(Properties,NP),
              append(IDTuple,Properties,Ghost_Arguments),
              GhostFact=..[G|Ghost_Arguments],
              %% writeln(call(db:interpretation(Ex,GhostFact))),
              call(db:interpretation(Ex,GhostFact)),
              %% writeln(call(db:interpretation(Ex,GhostFact))),
              %% writeln(Properties),
              %% writeln((CaseID,G,Properties)),
              forall( nth0(N,Properties,Feature),
                      ( atomic_list_concat([G,'_',N],FeatureStr),
                        feature_to_float(Feature,FeatureVal),
                        writeln(''),writeln('-------------------'),
                        writeln(CaseID),write_sparse_vector(CaseID),
                        writeln(add_feature_to_sparse_vector(CaseID,FeatureStr,FeatureVal)),
                        add_feature_to_sparse_vector(CaseID,FeatureStr,FeatureVal),
                        writeln(CaseID),write_sparse_vector(CaseID),
                        writeln('-------------------')
                      )
                    )
             )
           ),
    prolog_set_target_label(CaseID,Y),
    ( user:klog_reject(Case,TrainOrPredict) -> set_rejected(CaseID) ; true),
    true.


%% FIXME: this way can't work for sliced interpretations
generate_all(TargetSignature,Models,Feature_Generator) :-
    examples(Examples),
    forall( member(Ex,Examples),
            ( Ex=..[_IntId|[_SliceId]] -> % Sliced
                throw(error(klog_error, (generate_all(Examples), ' sliced data set')))
              ;
                true
            )
          ),
    klog_writeln(4,generate_all(TargetSignature,Examples,Models,Feature_Generator)),
    flatten(Models,ListOfModels),
    check_job(TargetSignature,ListOfModels),
    kill_present(TargetSignature,Examples),
    kill_future(Examples),
    debug_graphs(Examples,generate),
    forall( get_task(TargetSignature,TaskIndex,TaskName,TaskType),
            (klog_writeln(4,task(TargetSignature,TaskIndex,TaskName,TaskType)),
             nth0(TaskIndex,ListOfModels,Model),
             cases_loop(TargetSignature,Examples,Model,Feature_Generator,TaskName,TaskType,generate)
            )
          ),
    resuscitate_present_and_future(Examples),
    remap_indices,
    true.


induce_all(TargetSignature,Models) :-
    klog_writeln(4,induce_all(TargetSignature,Models)),
    examples(Examples),
    get_klog_flag(pthreshold,Pthreshold),
    flatten(Models,ListOfModels),
    forall( get_task(TargetSignature,TaskIndex,_TaskName,TaskType),
            ( nth0(TaskIndex,ListOfModels,Model),
	      forall( member(Ex,Examples),
		      ( forall( tuple_of_identifiers(Ex,TargetSignature,IDTuple),
                                ( make_case(Ex,TargetSignature,IDTuple,Case),
                                  make_case_id(Ex,TargetSignature,IDTuple,CaseID),
                                  (get_prediction(Model,CaseID,Margin) ->  induce_one(TargetSignature, Margin, Case, TaskType) ; kdebug(no_pred(CaseID)) ),
                                  true
                                  %% get_prediction(Model,CaseID,Margin),
                                  %% ( Margin > Pthreshold-> assert(induced(Case)) ; true )
                                %% FIXME: - assert whatever predicted by model!
                                ))
                      ))
            )),
    true.


% Lost... damn with git. Trying to reconstruct it
induce_one(TargetSignature,Margin,Case,binary_classification) :-
    term_variables(Case,Vars),
    ( length(Vars,0) ->
      get_klog_flag(pthreshold,Pthreshold),
      ( Margin > Pthreshold-> assert(induced(Case)) ; true )
    ;
      ( length(Vars,1) ->
        Yint is round(Margin),
        atomic_label(Y,Yint),
        [Y] = Vars,
        assert(induced(Case))
      ;
        throw(error(klog_error, (induce_one(TargetSignature,Margin,Case,binary_classification), ' binary classification with 2 or more properties?')))
      )
    ).
induce_one(TargetSignature,Margin,Case,multiclass_classification) :-
    term_variables(Case,Vars),
    ( length(Vars,1) ->
      Yint is round(Margin),
      atomic_label(Y,Yint),
      [Y] = Vars,
      assert(induced(Case)),
      kdebug(multiclass+assert(induced(Case))),
      true
    ;
      throw(error(klog_error, (induce_one(TargetSignature,Margin,Case,multiclass_classification), ' multiclass classification with # properties != 1?')))
    ),
    true.


:- dynamic atomic_label/2.
prolog_set_target_label(CaseID,Y) :-
        number(Y),set_target_label(CaseID,Y).
prolog_set_target_label(CaseID,Y) :-
        atom(Y),
        atomic_label_to_integer(Y,Yint), % for multiclass
        set_target_label(CaseID,Yint).
atomic_label_to_integer(Y,Yint) :-
        atomic_label(Y,Yint),!. % if already defined
atomic_label_to_integer(Y,Yint) :-
        atomic_label(_,Yi), % take the most recent one
        Yint is Yi+1,
        %% writeln(atomic_label(Y,Yint)),
        set_label_name(Y,Yint),
        asserta(atomic_label(Y,Yint)),!.
atomic_label_to_integer(Y,1) :-
        %% writeln(atomic_label(Y,1)),
        set_label_name(Y,1),
        asserta(atomic_label(Y,1)),!.

declare_atomic_labels(List) :-
    dat(List,1).
dat([],_).
dat([Y|T],N) :-
    asserta(atomic_label(Y,N)),
    set_label_name(Y,N),
    N1 is N+1,
    dat(T,N1).
    
    

get_case_label(Case,callme,Y) :-
        ( call(Case) -> Y=1.0 ; Y = -1.0 ),
        %% writeln(got_case_label(Case,Y)),
        %writeln(case(Case)),
        !.
get_case_label(Case,_PropName#Pos,Y) :-
        %writeln(case(Case)),
        call(Case),
        db:interpretation(_,Fact)=Case,
        Fact=..[_Functor|Args],
        nth0(Pos,Args,Y),
        %writeln(case(Case)),
        !.


atomize_interpretation_list([],[]) :- !.
atomize_interpretation_list([H|T],[H|AT]) :-
        atomic(H),
        atomize_interpretation_list(T,AT),!.
atomize_interpretation_list([Ex|T],[IntId_SliceId|AT]) :-
        Ex=..[IntId,SliceId],
        add_separators([IntId,SliceId],Sep),
        atomic_list_concat(Sep,IntId_SliceId),
        atomize_interpretation_list(T,AT),!.
atomize_interpretation_list([H|_],_) :-
        throw(error(klog_error, (atomize_interpretation_list(H,_), 'Invalid ex spec'))).



make_case(Ex,S,IDTuple,db:interpretation(Ex,G)) :-
	replace_properties_by_vars(S,IDTuple,Args),
	G=..[S|Args].

replace_properties_by_vars(S,IDTuple,Args) :-
	signature_traits(S,column_types,Types),
	rep_by_vars(IDTuple,Types,Args).
	
rep_by_vars([],[],[]).
rep_by_vars(IDTuple,[property|TRest],[_|PRest]) :-
	rep_by_vars(IDTuple,TRest,PRest).
rep_by_vars([A|ARest],[T|TRest],[A|PRest]) :-
	\+ (T = property),
	rep_by_vars(ARest,TRest,PRest).

	
%% tuple_of_identifiers(+Ex,+S,-List) is nondet.
%
% Unify IDList with a tuple of identifiers in Ex whose type appears in
% signature S. On backtracking will retrieve all possible tuples. For
% tasks such as link prediction, this predicate is used to generate
% all pairs of candidates.
tuple_of_identifiers(Ex,S,IDs) :-
	extract_references(S,Types),
	rtoi(Ex,Types,IDs).
pick_check(S,IDs) :-
        atomic_concat(pick_,S,PickName),
        PickPred=..[PickName|IDs],
        call(user:PickPred).
rtoi(_,[],[]).
rtoi(Ex,[Type|Types],[ID|IDs]) :-
	identifier(Ex,Type,ID),
	rtoi(Ex,Types,IDs).
	
%% identifier(+Ex,+S,-ID) is nondet
% Unify ID with one of the identifiers of S in Ex. On backtracking
% will return all data identifiers for signature S in interpretation
% Ex. The predicate fails if S is not an entity.
identifier(Ex,S,ID) :-
	signature_traits(S,kind,entity),
	signature_traits(S,arity,A),
	length(Args,A),
	Goal=..[S|Args],
	call(db:interpretation(Ex,Goal)),
	extract_identifiers(S,Args,[ID]).



%%%%%%%%%% wrappers handling sliced and non-sliced interpretations


% This is a little hack to save memory used by the feature
% generator. Once an interpretation has been processes, some internal
% data structures can be deallocated.
prolog_clean_internals(Ex) :-
    atomic(Ex),
    clean_internals(Ex).
%% sliced interpretations can't be really cleaned this way In all data
%% sets I know cleaning here would be unnecessary. However if there
%% are many sliced interpretations memory consumption could be an
%% issue again. FIXME: maybe forall(member(Ex,Examples) in cases_loop
%% should be split into an outer loop on interpretations and an inner
%% loop on slices. Will do when I can think of a data set where this
%% is useful.
prolog_clean_internals(Ex) :-
    Ex=..[_IntId,_SliceId],
    true.
%% clean_internals(IntId).
prolog_clean_internals(Ex) :-
    throw(error(klog_error, (prolog_clean_internals(Ex), 'Invalid ex spec'))).

%% prolog_make_sparse_vector(Model,Feature_Generator,Ex,CaseID,ViewPoint) is det
%
% Wrapper around C++ method for making feature vectors.
% Feature_Generator is the name of the feature generator object that
% will be used. Ex is the interpretation name, possibly sliced. CaseID
% identifies the case for which the feature vector is
% generated. ViewPoint is a list of vertex IDs (C++ integer code)
% around which the feature vector is constructed. For unsliced
% interpretations, the feature vector is registered under a
% slash-separated string like ai/advised_by/person20/person240,
% created from the interpretation identifier (e.g. ai) followed by the
% target signature (e.g. advised_by) and the identifiers of the
% entities that define the case, (e.g. person20 and person412). For
% sliced interpretations the slice name is also used to construct the
% identifiers, e.g. imdb_1997 and imdb_1997/m441332. Model is used to
% determine the internal format of the feature vector being generated.

prolog_make_sparse_vector(Model,Feature_Generator,Ex,CaseID,ViewPoint) :-
    %% writeln(prolog_make_sparse_vector(Model,Feature_Generator,Ex,CaseID,ViewPoint)),
        atomic(Ex),
        model_type(Model,ModelType),
        make_sparse_vector(ModelType,Feature_Generator,Ex,Ex,CaseID,ViewPoint),
        true.
        
prolog_make_sparse_vector(Model,Feature_Generator,Ex,CaseID,ViewPoint) :-
        ( Ex=..[IntId,SliceId] -> true
        ;
          throw(error(klog_error,
                      (prolog_make_sparse_vector(Model,Feature_Generator,Ex,CaseID,ViewPoint),
                       'Invalid ex spec')))
        ),
        add_separators([IntId,SliceId],Sep),
        atomic_list_concat(Sep,IntId_SliceId),
        model_type(Model,ModelType),
        make_sparse_vector(ModelType,Feature_Generator,IntId,IntId_SliceId,CaseID,ViewPoint).


%% make_case_id(Ex,S,IDTuple,CaseID) is det
%
% Unifies CaseID with a unique identifier for (sliced) interpretation
% Ex, target signature S, and tuple of identifiers IDTuple. See
% prolog_make_sparse_vector/5 for details on how this is formatted.

make_case_id(Ex,S,IDTuple,CaseID) :-
        Ex=..ExList, % works both on atoms and simple terms - at this
                     % point Ex is surely correct, no need for
                     % exceptions here
        flatten([ExList,S|IDTuple],F),
        add_separators(F,FS),
        atomic_list_concat(FS,CaseID).

add_separators([],[]).
add_separators(S,SepS) :- recursive_add_separators(S,SepS).
recursive_add_separators([X],[X]).
recursive_add_separators([L|R],[L,'/'|RS]) :-
        recursive_add_separators(R,RS).


fix_interpretation_identifiers([],[]).
fix_interpretation_identifiers([Ex|Rest],[IntId_SliceId|Rest2]) :-
        Ex=..IntId_SliceId_List,
        add_separators(IntId_SliceId_List,IntId_SliceId_Sep_List),
        atomic_list_concat(IntId_SliceId_Sep_List,IntId_SliceId),
        fix_interpretation_identifiers(Rest,Rest2).


%% save_induced_facts(Signatures,Filename) is det
%
% Every 'induced' fact (asserted during test) is saved to Filename for
% a rough implementation of iterative relabeling. The target signature
% is renamed by prefixing it with 'pred_'.

save_induced_facts(Signatures,Filename) :-
    flatten(Signatures,SigList),
    open(Filename,write,Stream),
    forall( member(Sig,SigList),
            ( forall( learn:induced(db:interpretation(Ex,Fact)),
                      ( Fact=..[Sig|Args] -> % One of the signatures to be saved
                        ( atomic_list_concat([pred_,Sig],NewSig),
                          NewFact=..[NewSig|Args],
                          NewCase = interpretation(Ex,NewFact),
                          format(Stream,'~w.~n',[NewCase])
                        )
                      ;
                        true
                      )
                    )
            )
          ),
    close(Stream).


kill_signature_in_slice(IntId,TargetSignature,SliceID) :-
    set_signature_in_slice_aliveness(IntId,TargetSignature,SliceID,false).
resuscitate_signature_in_slice(IntId,TargetSignature,SliceID) :-
    set_signature_in_slice_aliveness(IntId,TargetSignature,SliceID,true).
kill_signature(IntId,TargetSignature) :-
    set_signature_aliveness(IntId,TargetSignature,false).
resuscitate_signature(IntId,TargetSignature) :-
    set_signature_aliveness(IntId,TargetSignature,true).
kill_slice(IntId,SliceId) :-
    set_slice_aliveness(IntId,SliceId,false).
resuscitate_slice(IntId,SliceId) :-
    set_slice_aliveness(IntId,SliceId,true).
