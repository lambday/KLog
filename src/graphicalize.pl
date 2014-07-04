/*
Graphicalization procedures

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

:- module(graphicalize,
	  [ map_prolog_ids_to_vertex_ids/3,
	    attach/0,
	    attach/1,
	    detach/0,
	    make_graphs/0,
	    check_db/0,
	    examples/1,
	    database/0,
	    database/1,
	    % assert_example/2,
	    % retract_example/1, % paolo - for pick
	    assert_background_knowledge/1,
            domain_identifiers/3
	  ]).

:- ensure_loaded('flags').
:- ensure_loaded('timing').
:- ensure_loaded('goals').
:- ensure_loaded('utils').
:- ensure_loaded('db').
:- ensure_loaded('repair').
:- use_module('C/c_interface').
:- ensure_loaded('syntax').

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(random)).

/** <module> Convert deductive databases into graphs

  This module contains predicates for converting a set of deductive
  databases into a set of graphs, one for each interpretation, using
  the mapping procedures described below.

  $ Concept:

  First the database is converted into a collection of ground facts by
  computing intensional tables. Then the graphicalizer generates a
  graph for each interpretation.

  $ Database:

  The database is assumed to be in the following 'E/R' normal form:
  
  * Attributes are either object identifiers or properties.
  
  * For each identifier i, there is at least one table t such that i
    is the primary key of t. This table is basically the class or
    entity-set of i.

  * For each table, the primary key only consists of identifiers. So
    multiway relationships are always represented by a table keyed by
    the object identifiers involved in the relationship, augmented
    with properties of the relationship.

  $ Graphicalization strategy:

  * There is a vertex for each entity tuple (called an i-vertex), and
  a vertex for each relationship tuple (an r-vertex). Vertices are
  labeled by their tuples. Note that because of the above assumptions
  there is one and only one i-vertex for each identifier.

  * There is an edge between an i-vertex and an r-vertex if i belongs
  to the tuple r. The graph is bipartite and there are no edges
  between vertices of the same kind.
  
  $ Usage:

  The module provides two fundamental predicates: attach/1, for
  loading a data set (variant attach/0 for toy datasets asserted
  directly in the kLog script), and make_graphs/0, make_graphs/1 for
  generating graphs.

  $ History:

  git log

@tbd Safety checks, assumption checks, throwing exceptions, performance tuning
@author Paolo Frasconi

*/

% --------------------------------------------------------------------
:- dynamic db:interpretation/2.
:- dynamic vertex/2, edge/2, id_2_index/2.
% --------------------------------------------------------------------


%% attach(+DataFile:atom_or_list_of_atoms) is det.
%
% Load a dataset i.e. a set of database instances (or interpretations)
% by reconsulting the given file(s). Any previously attached data set
% will be internally cleaned up so only one data set can be attached
% at a time. Internally the predicate just consults the file
% containing declarations of the predicate db:interpretation/2 (a
% collection of ground facts). The predicate is stored in the
% 'graphicalize' module and should be never necessary to inspect it
% from outside this module.

%% attach is det.
%
% This first form is useful to attach a toy dataset written directly in
% the kLog script. The data need to be declared as
%
% ==
% db:interpretation(Ex,Fact).
% ==


:- dynamic number_of_extensional_facts/1, number_of_facts/1, attached_datafile/1.
% NB: nothing prevents passing a list of files here... consult/1 accepts a list
attach(DataFile) :-
        cleanup_data, % cleanup graphs and sparse vectors
	use_timer( load_dataset,db_consult(DataFile) ),
        retractall(attached_datafile(_)),
        klog_color_format(3,'Loaded ~w ', [DataFile],green),
        attach,
        assert(attached_datafile(DataFile)).
        
attach :- % calling this version is useful for attaching a small
          % database declared in the kLog script, mostly for
          % debugging/testing purposes
	( attached_datafile(_) ->
	  throw(error(klog_error,
                      (attach, 'Cannot attach/0 after attach/1 - Quit kLog and fix the script')))
	;
	  true
	),
        extensional_facts(ExtFacts),
        length(ExtFacts,NExtFacts),
        retractall(number_of_extensional_facts(_)),
        assert(number_of_extensional_facts(NExtFacts)),      
        interpretations(Int_IDs),
        length(Int_IDs,N),
        klog_color_format(3,'(~w interpretations, ~w extensional facts)~n',
                     [N,NExtFacts],green),
        klog_color_format(3,'Computing intensional tables~n',[],yellow),
        %unknown(OldHandler,fail), % Prevent "undefined predicate" messages
	assert_background_knowledge(Int_IDs),
        %unknown(_,OldHandler), % restore undefined predicate handler
        ( get_klog_flag(check_duplicate_ids,yes) ->
          check_db % must be called only after BK asserted!!!
          ;
          true
        ),
	bagof(Fact, Ex^(db:interpretation(Ex,Fact)), AllFacts), length(AllFacts,NAllFacts),
	klog_color_format(3,'Intensional tables done: ~w total facts~n', [NAllFacts],green),
        retractall(number_of_facts(_)),
        assert(number_of_facts(NAllFacts)),
        get_klog_flag(referential_integrity_repair,RepairMode),
        ( member(RepairMode,[add,delete]) ->
          klog_color_format(3,'Searching identifiers for referential integrity check~n',[],yellow),
          use_timer(searching_identifiers,search_identifiers),
          klog_color_format(3,'Constants found~n',[],green),
          klog_color_format(3,'Checking referential integrity~n',[],yellow),
          forall( member(Int_ID,Int_IDs), use_timer(repairing_ric,check_and_repair(Int_ID)) ),
          klog_color_format(3,'Referential integrity done~n',[],green)
        ;
          true
        ),
        make_graphs,
        learn:record_dependencies.

%% detach is det.
%
% Retracts all data ground atoms and results of graphicalization.
detach :-
    interpretations(Int_IDs),
    klog_color_format(3,'Detaching...:~n',[],yellow),
    forall(member(Int_ID,Int_IDs), delete_graph(Int_ID)),
    retractall(db:interpretation(_,_)),
    retractall(attached_datafile(_)),
    retractall(vertex_map(_,_,_)).

extensional_facts([]) :- \+ db:interpretation(_,_).
extensional_facts(ExtFacts) :-
        bagof(Fact, Ex^(db:interpretation(Ex,Fact)), ExtFacts).

% --------------------------------------------------------------------

%% examples(-Set:list_of_atoms) is det.
% 
% Returns in Set the interpretation identifiers in the attached domain.
examples([]) :- \+ example(_).
examples(D) :-
	setof(Ex,example(Ex),D).
example(Ex) :-
	db:interpretation(Ex,_Fact).
interpretation(Int_ID) :-
        db:interpretation(Int_ID,_Fact),
        atomic(Int_ID).
interpretation(Int_ID) :-
        db:interpretation(Ex,_Fact),
        Ex=..[Int_ID,_Slice_ID].
interpretations(Int_IDs) :-
	setof(Int_ID,interpretation(Int_ID),Int_IDs).

slices(Int_ID,Slice_IDs) :-
        setof(Slice_ID, Ex^Fact^(Ex=..[Int_ID,Slice_ID],db:interpretation(Ex,Fact)), Slice_IDs).
slices(_Int_ID,[]).

%% database is det.
% 
% For debugging purposes. List the extensional database.
database :-
	listing(db:interpretation/2).
%% database(?Ex) is det.
% 
% For debugging purposes. List the extensional database of interpretation Ex.
database(Ex) :-
	forall(db:interpretation(Ex,Fact), format('interpretation(~w,~w).~n',[Ex,Fact])).
	
% --------------------------------------------------------------------

%% assert_background_knowledge(+Dataset:list_of_atoms) is det
%
% Deduce intensional facts from the background knowledge and
% interpretations listed in Dataset and assert all of them (at the
% top) in the form db:interpretation(Ex,Fact).  Once asserted in this
% way, intensional and extensional groundings are
% indistinguishable. Use the first form to assert background knowledge
% for every interpretation in the domain.

assert_background_knowledge([]) :-
        use_timer( background_knowledge,
                   forall( signature_traits(_,ext_clause,Clause),
                           ( (Clause = (Head :- _Body) ; Clause = Head),
                             assert_bk(db,Head) %,
                           )
                         )
                 ).
% Takes in list of interpretations, can be either sliced or not sliced
assert_background_knowledge(Int_IDs) :-
	forall( member(Int_ID,Int_IDs),
                ( slices(Int_ID,Slice_IDs),
                  use_timer(background_knowledge,assert_background_knowledge(Int_ID,Slice_IDs))
                )
	      ).
% Empty slice list case (this is for unsliced interpretations)
assert_background_knowledge(Int_ID,[]) :-
        bagof(Fact, db:interpretation(Int_ID,Fact), Facts),
        forall(member(F,Facts), assert(user:F)),
        forall( signature_traits(_,ext_clause,Clause),
                ( (Clause = (Head :- _Body) ; Clause = Head),
                  assert_bk(Int_ID,Head) %,
                )
              ),
	forall(member(F,Facts), retract(user:F)).

% Otherwise do each slice recursively
assert_background_knowledge(Int_ID,Slice_IDs) :-
        abk(Int_ID,Slice_IDs).

abk(_Int_ID,[]).
abk(Int_ID,[Slice_ID|Other_Slice_IDs]) :-
        Ex=..[Int_ID,Slice_ID],
        bagof(Fact, db:interpretation(Ex,Fact), Facts),
        forall(member(F,Facts), assert(user:F)),
        forall( signature_traits(_,ext_clause,Clause),
                ( (Clause = (Head :- _Body) ; Clause = Head),
                  assert_bk(Ex,Head) %,
                )
              ),
        abk(Int_ID,Other_Slice_IDs),
	forall(member(F,Facts), retract(user:F)).



assert_bk(Ex,Fact) :-
        klog_writeln(6,called_assert_bk(Ex,Fact)),
        call(user:Fact),
	( ground(user:Fact) ->
	  true
	;
	  atomic_list_concat(['Call resulted in a non ground fact!'],Message),
	  throw(error(klog_error,(assert_bk(Ex,Fact), Message)))
	),
	( already_asserted(Ex,Fact) ->
                                %db:interpretation(Ex,Fact) -> % FIXME: First check that all other data sets still work. Then this line must be replace so that if a fact is already asserted in a slice preceq this slice it is not asserted again. Then there is also to modify other predicates where examples/1 was used, e.g. when checking referential integrity constraints - BTW new condition on referential integrity of slices must be checked!!
	  true % might have been proved in different ways but it's ok..
	; % check for primary keys (A.4 in the paper)
          properties_to_vars(Fact,LiftedFact),
          ( already_asserted_entity(Ex,LiftedFact) -> % !! Fix
            %db:interpretation(Ex,LiftedFact) ->
            atomic_list_concat(['Duplicate key!'],Message),
            throw(error(klog_error,(assert_bk(Ex,Fact), Message)))
          ;
            asserta(db:interpretation(Ex,Fact)),
            %writeln(asserta(db:interpretation(Ex,Fact))),
            true
          )
	),
%	write(result(user:Fact)),nl,
	fail.
assert_bk(_Ex,_Fact).


already_asserted_entity(Ex,LiftedFact) :-
        LiftedFact=..[S|_],
        signature_traits(S,kind,entity),
        already_asserted(Ex,LiftedFact).

already_asserted(Ex,Fact) :-
        % writeln(already_asserted(Ex,Fact)),
        ( atomic(Ex) ->
          db:interpretation(Ex,Fact)
        ;
          ( Ex=..[Int_ID,Slice_ID] ->
            db:slice_preceq(Prev_Slice_ID,Slice_ID),
            Prev_Ex=..[Int_ID,Prev_Slice_ID],
            db:interpretation(Prev_Ex,Fact)
          ;
            throw(error(klog_error,(already_asserted(Ex,Fact), 'Invalid spec')))
          )
        ).


retract_bk(Ex,Fact) :-
	call(user:Fact),
	retract(db:interpretation(Ex,Fact)),
	fail.
retract_bk(_Ex,_Fact).

% Change every property value in ground fact Fact into a variable and
% build a corresponding lifted fact for checking the uniqueness of
% primary keys.

properties_to_vars(Fact,LiftedFact) :-
        Fact=..[Name|Args],
        signature_traits(Name,column_types,T),
	p2v(Args,T,LiftedArgs),
        LiftedFact=..[Name|LiftedArgs].
p2v([],[],[]).
p2v([_A|ARest],[property|TRest],[Var|LARest]) :-
        var(Var),
	p2v(ARest,TRest,LARest).
p2v([A|ARest],[T|TRest],[A|LARest]) :-
	\+ (T = property),
	p2v(ARest,TRest,LARest).



% --------------------------------------------------------------------

%% save_view(+Filename) is det.
%
% Save a view of the data using only declared signatures. This allows
% to write kLog scripts for data set transformation.

save_view(Filename) :-
    domain_traits(signatures,Sigs),
    bagof((Int_ID,Fact), S^A^(db:interpretation(Int_ID,Fact),Fact=..[S|A],member(S,Sigs)), Facts),
    open(Filename,write,Stream),
    forall(member((I,F),Facts),
           format(Stream,'interpretation(~w,~w).~n',[I,F])),
    close(Stream).

% --------------------------------------------------------------------

%% make_graphs is det.
%
% Graphicalize the attached interpretations. Graphs are stored in
% memory. 
make_graphs :-
        interpretations(Int_IDs),
	klog_color_format(3,'Graphicalization:~n',[],yellow),
        use_timer( making_graphs,
                   ( add_graphs(Int_IDs),
                     add_entity_vertices,
                     add_relationship_vertices_and_edges
                   )
                 ),
	klog_color_format(3,'~nGraphicalization ended~n',[],green),
        report_timings(Total),
	number_of_facts(NFacts),
	Ratio is NFacts / Total,
	klog_color_format(3,'Processed ~2f facts/sec ~n',[Ratio],blue).
	

% --------------------------------------------------------------------
:- dynamic vertex_map/3.

%% vertex_map(Ex:atom,SymID:atom,NumID:integer) is det
%
% Dynamically asserted. Dictionary mapping a symbolic vertex
% identifier (used in Prolog) to its numeric identifier in the
% internal C++ representation of the graph. The symbolic identifier is
% defined as follows. If v is an i-vertex, then SymID is the tuple
% identifier. If v is an r-vertex, then SymID is the concatenation of
% the signature name and the identifiers contained in it.

assert_vertex_map(Ex,ThisSymbolicID,ThisNumericID) :-
        ( atomic(Ex) -> assert(vertex_map(Ex,ThisSymbolicID,ThisNumericID))
        ;
          ( Ex=..[Int_ID,_Slice_ID] ->
            assert(vertex_map(Int_ID,ThisSymbolicID,ThisNumericID))
          ;
            throw(error(klog_error, (assert_vertex_map(Ex,ThisSymbolicID,ThisNumericID), 'Invalid ex spec')))
          )
        ).

read_vertex_map(Ex,ForeignID,ForeignNumericID) :-
%        writeln(read_vertex_map(Ex,ForeignID,ForeignNumericID)),
        ( atomic(Ex) ->
          vertex_map(Ex,ForeignID,ForeignNumericID)
        ;
          ( Ex=..[Int_ID,_SliceId] ->
            ( vertex_map(Int_ID,ForeignID,ForeignNumericID) -> true
            ;
              fail
            )
          ;
            throw(error(klog_error, (read_vertex_map(Ex,ForeignID,ForeignNumericID), 'Invalid ex spec')))
          )
        ).

%% add_graphs(+D:list_of_atoms) is det.
%
% For each interpretation id in D, create a new empty C++ Graph object
add_graphs(Int_IDs) :-
        forall(member(Int_ID,Int_IDs), add_graph(Int_ID)).

% --------------------------------------------------------------------

%% add_entity_vertices is det
%
% Loop through active E-relations and add the corresponding vertices
% to the internal collection of graphs (this is step 1 of graphicalization).
add_entity_vertices :-
        number_of_facts(N), new_progress_bar(aev,N),
	domain_traits(active_entities,Entities),
        forall( (db:interpretation(Ex,Fact)),
                ( progress_bar(aev),
                  ensure_ground(Fact),
                  %writeln(db:interpretation(Ex,Fact)),
                  Fact=..[S|Args],
                  ( member(S,Entities) ->
                    is_kernel_point(S,IsKP),
                    IsAlive = yes,
                    extract_properties(S,Args,Properties),
                    extract_identifiers(S,Args,Identifiers),
                    atomic_list_concat(Identifiers,ThisSymbolicID), % (only one..)
                    prolog_add_vertex(Ex,[S|Properties],IsKP,IsAlive,i,ThisSymbolicID,ThisNumericID),
                    assert_vertex_map(Ex,ThisSymbolicID,ThisNumericID)
                  ;
                    true        % skip over relationships
                  )
                )
              ),
        klog_format(3,'~nEntity nodes done~n',[]).

% --------------------------------------------------------------------

%% add_relationship_vertices_and_edges is det
%
% Loop through active R-relations and add the corresponding vertices
% to the internal collection of graphs. Additionally, add edges to the
% collection of graphs  (this is step 2 of graphicalization).
add_relationship_vertices_and_edges :-
        number_of_facts(N), new_progress_bar(arve,N),
	domain_traits(active_relationships,Relationships),
        forall( (db:interpretation(Ex,Fact)),
                ( progress_bar(arve),
                  ensure_ground(Fact),
                  %% writeln(db:interpretation(Ex,Fact)),
                  Fact=..[R|Args],
                  ( member(R,Relationships) ->
                    is_kernel_point(R,IsKP),
                    IsAlive = yes,
                    extract_properties(R,Args,Properties),
                    %% writeln(extract_properties(R,Args,Properties)),
                    extract_identifiers(R,Args,Identifiers),
                    atomic_list_concat([R|Identifiers],ThisSymbolicID),
                    prolog_add_vertex(Ex,[R|Properties],IsKP,IsAlive,r,ThisSymbolicID,ThisNumericID),
                    assert_vertex_map(Ex,ThisSymbolicID,ThisNumericID),
                    forall( member(ForeignID,Identifiers),
                            ( ( read_vertex_map(Ex,ForeignID,ForeignNumericID) ->
                                nth0(Position,Args,ForeignID),
                                signature_traits(R,column_roles,ColumnRoles),
                                nth0(Position,ColumnRoles,Role),
                                prolog_add_edge(Ex,ThisNumericID,ForeignNumericID,[Role],_)
                              ; % else it an integrity violation
                                klog_flag(referential_integrity_repair,Repair),
                                ( member(Repair,[delete,add]) ->
                                  atomic_list_concat(['Failed to retrieve numeric ID for vertex ',
                                                      ForeignID],Message),
                                  throw(error(klog_error,
                                              ((add_relationship_vertices_and_edges(Ex),fact(Fact)),
                                               Message)))
                                ;
                                  true
                                )
                              )
                            )
                          )
                  ;
                    true        % skip over entities
                  )
                )
              ),
        klog_format(3,'~nRelationship nodes done~n',[]).

% wrapper to add_vertex, handle sliced and non-sliced interpretations
prolog_add_vertex(Ex,Label,IsKernelPoint,IsAlive,Kind,SymId,Id) :-
        atomic(Ex), add_vertex(Ex,bottom_slice,Label,IsKernelPoint,IsAlive,Kind,SymId,Id).
prolog_add_vertex(Ex,Label,IsKernelPoint,IsAlive,Kind,SymId,Id) :-
        Ex=..[IntId,SliceId],
        atomic_list_concat([SliceId],AtomSliceId),
        add_vertex(IntId,AtomSliceId,Label,IsKernelPoint,IsAlive,Kind,SymId,Id). % call C++
prolog_add_vertex(Ex,Label,IsKernelPoint,IsAlive,Kind,SymId,Id) :-
        throw(error(klog_error, (prolog_add_vertex(Ex,Label,IsKernelPoint,IsAlive,Kind,SymId,Id), 'Invalid ex spec'))).

% wrapper to add_edge, handle sliced and non-sliced interpretations
prolog_add_edge(Ex,V,U,Label,Id) :-
        atomic(Ex), add_edge(Ex,V,U,Label,Id).
prolog_add_edge(Ex,V,U,Label,Id) :-
        Ex=..[IntId,_SliceId],
        add_edge(IntId,V,U,Label,Id).
prolog_add_edge(Ex,V,U,Label,Id) :-
        throw(error(klog_error, (prolog_add_edge(Ex,V,U,Label,Id), 'Invalid ex spec'))).

ensure_ground(Fact) :-
	( ground(Fact) ->
	  true
	;
	  atomic_list_concat(['Fact not ground!'],Message),
	  throw(error(klog_error,(gl((Fact)), Message)))
	).


% --------------------------------------------------------------------

%% map_prolog_ids_to_vertex_ids(+Ex:atom,+IDs:[atom],-NumericIds:[integer])
%
% Retrieve numeric vertex IDs for a list of database identifiers IDs
% in interpretation Ex
map_prolog_ids_to_vertex_ids(_,[],[]).
map_prolog_ids_to_vertex_ids(Ex,[ID|IDs],[NumericID|NumericIDs]) :-
	read_vertex_map(Ex,ID,NumericID),
	map_prolog_ids_to_vertex_ids(Ex,IDs,NumericIDs).



esetof(X,P,B) :- setof(X,P,B).
esetof(_,_,[]).


%% domain_identifiers(?Ex,?Type,?Constants) is det
% 
% True if Constants are the constants of type Type in interpretation Ex.

:- dynamic domain_identifiers/3.

% --------------------------------------------------------------------

%% search_identifiers is det
%
% search identifiers from the ground entity sets. Results are asserted in
% the database as domain_identifiers/3.

search_identifiers :-
        number_of_facts(N), new_progress_bar(search_ids,N),
        retractall(domain_identifiers(_,_,_)),
        domain_traits(active_entities,Entities),
        forall( db:interpretation(Ex,Fact),
                ( progress_bar(search_ids),
                  Fact=..[S|Args],
                  ( member(S,Entities) ->
                    ( signature_traits(S,column_types,ColumnTypes),
                      nth0(Position,ColumnTypes,self),
                      nth0(Position, Args, Val),
                      ( domain_identifiers(Ex,S,Bag) ->
                        retract(domain_identifiers(Ex,S,Bag))
                      ;
                        Bag=[] ),
                      append(Bag,[Val],NewBag),
                      assert(domain_identifiers(Ex,S,NewBag)),
                      true
                    )
                  ;
                    true
                  ),
                  true
                )
              ).


:- dynamic duplicate_id/3.

% TODO: kLog gives no error if a constant is used as identifier for
% two different entity sets!

%% check_db is det
%
% Internal use. Performs some checks on the loaded data sets.
check_db :-
        klog_writeln(3,'checking db'),
        bagof(Ex,db:interpretation(Ex,_),Dataset),
        setof(Ex,db:interpretation(Ex,_),UniqDataset),
        length(Dataset,N),
        length(UniqDataset,Nu),
        ( Nu < N ->
	  atomic_list_concat(['Attached domain has duplicate interpretation IDs\n'],Message),
          throw(error(klog_error,
                      (bag_vs_set_of_ids(Dataset,UniqDataset), Message)))
        ;
          true
        ),
	retractall(duplicate_id(_,_,_)),
	domain_traits(entities,Entities),
	forall( member(S,Entities),
		( signature_traits(S,column_types,ColumnTypes),
		  nth0(Position, ColumnTypes, self),
		  signature_traits(S,arity,Arity),
		  klog_format(5,'Checking ~w/~w~n',[S,Arity]),
		  forall( member(Ex,Dataset),
			  ( length(Args,Arity), % a list of free variables
			    nth0(Position, Args, ID), % get the variable corresponding to the identifier
			    delete(Args,ID,Other), % so Other now is a list of non-ID variables
			    Fact=..[S|Args],
			    bagof(ID,Other^(db:interpretation(Ex,Fact)),Bag),
			    find_duplicates(S,Ex,Bag),
			    true
			  )
			)
		)
	      ),
	( duplicate_id(_,_,_) ->
	  atomic_list_concat(['Attached domain has duplicate identifiers'],Message),
	  bagof((entity:S,interpretation:Ex,id:ID),duplicate_id(S,Ex,ID),Duplicates),
	  throw(error(klog_error,
                      (non_unique_identifiers(Duplicates), Message)))
	;
	  true
	)
	.

find_duplicates(S,Ex,List) :-
        msort(List,SList),
        dup(S,Ex,SList).
dup(S,Ex,[ID,ID|Tail]) :- assert(duplicate_id(S,Ex,ID)), dup(S,Ex,[ID|Tail]).
dup(S,Ex,[_Head|Tail]) :- dup(S,Ex,Tail).
dup(_S,_Ex,[]).
