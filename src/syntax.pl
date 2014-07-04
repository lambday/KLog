/*
Syntactic extensions to Prolog

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

:- module(syntax,[ domain_traits/0,
		   domain_traits/1,
		   domain_traits/2,
		   signature_traits/3,
		   id_position/2,
		   extract_properties/3,
		   extract_identifiers/3,
		   extract_references/2,
		   is_kernel_point/2,
		   op(700,xfx,::),
		   op(500,fx,signature), %
		   op(500,xfx,:=), %
		   op(500,fx,predict),
		   op(200,fy,*), %
		   op(500,xfx,@) %
		 ]).

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(random)).

:- ensure_loaded('flags').
:- ensure_loaded('utils').
:- ensure_loaded('goals').
:- ensure_loaded('reachability').

:- op(700,xfx,::).
:- op(500,fx,signature).
:- op(500,fx,predict).
:- op(200,fy,*).
:- op(500,xfx,@).
:- op(100,xfx,:=).

:- dynamic (signature)/1.
:- dynamic signature_traits/3.
:- dynamic domain_traits/2.


/** <module> kLog syntax

  This module provides syntactic extensions to Prolog for declaring
  signatures. Most of the functionalities in this module are
  implemented via term_expansion/2.

  $ History:

  see git log

@tbd Safety checks, assumption checks, performance tuning
@author Paolo Frasconi

*/


%% signature(+S) is det
%
% Declare S to be the type signature of a table, according to the following syntax:
% ==
%    <signature> ::= <header> [<sig_clauses>]
%    <sig_clauses> ::= <sig_clause> | [<sig_clause> <sig_clauses>]
%    <sig_clause> ::= <Prolog_clause>
%    <header> ::= <sig_name> "(" <args> ")" "::" <level> "."
%    <sig_name> ::= <Prolog_atom>
%    <args> ::= <arg> | [<arg> <args>]
%    <arg> ::= <column_name> [<role_overrider>] "::" <type>
%    <column_name> ::= <Prolog_atom>
%    <role_overrider> ::= "@" <role>
%    <role> ::= <Prolog_atom>
%    <type> ::= "self" | <sig_name>
%    <level> ::= "intensional" | "extensional"
% ==
%
% Arguments are given by default a role that corresponds to their
% position in the argument list. The role can be overridden using the
% @ syntax, e.g. to represent undirected relations. By default, the
% role of the i-th argument is: * functor+i if the argument is an
% identifier * functor+t if the argument is a property and t its type
%
% thus, e.g. if a_id and b_id are identifier names, and c_type is a
% property name, the following signatures are equivalent:
%
% ==
% signature foo(g_id::gnus,t_id::tnus,c::property)::extensional.
% signature foo(g_id@1,t_id@2,c@3::property)::extensional.
% ==
%
% Forcing the role can also easily implement undirected edges.  Here is an
% example from mutagenesis:
%
% ==
% signature atm( atom_id::self,
%                element::property,
%		 quantaval::property,
%		 chargeval::property).
% signature bond( atom_id@b::atom,
%                 atom_id@b::atom,
%		  bondtype::property).
% ==
%
% More complex cases can be easily conceived, e.g. one can assign the
% same role to every atom in a benzene ring but distinguish different
% rings in a functional group like ball3 or phenanthrene. See
% experiments/mutagenesis for details.

%% expand_signature(+S,+Level) is det
%
% Auxiliary predicate for expanding a signature declaration where S is
% a fact with typed arguments and Level is either 'intensional' or
% 'extensional'. This predicate performs a number of checks and
% asserts various signature traits (see below). 
% term_expansion/2.

expand_signature(S,Level) :-
        %write('term_expanding '), write((signature S)::Level), nl,
	S=..[Name|Args],
	( memberchk(_ID::self,Args) ->  Kind = entity ; Kind = relationship ),
	length(Args,NArgs),
	( current_signature(CurrentName/_CurrentArity) ->
	  %write(current_signature(CurrentName/CurrentArity)),nl,
	  % first of all, conclude the current signature, i.e.
	  % check that there are extensionalizers if intensional
	  ( extension_ok(CurrentName) ->
	    true
	  ;
	    atomic_list_concat(['signature >',CurrentName,'< is intensional but has no extensionalizer'],Message),
	    throw(error(klog_error,
	     		(term_expansion((signature S)::Level), Message)))
	  )
	;
	  true
	),
	retractall(current_signature(_/_)),
	assert(current_signature(Name/NArgs)),
	length(Args,Arity),
	( Level = intensional ->
          % tabling here crashes yap
%	  (table user:Name/Arity),
          true,
	  (dynamic user:Name/Arity),
          true % FIXME: can't remember reason for dynamic above
	;
	  ( Level = extensional ->
	    true
	  ;
	  atomic_list_concat(['Invalid level specification in signature: >',Name,'/',Arity,'< : >',Level,'<'],Message),
	  throw(error(klog_error,
		      (term_expansion((signature S)::Level), Message)))
	  )
	),
% 	% prolog_load_context(term_position,TermPosition),
% 	%write('Entering '), write(expand_signature(Signature,Kind,Level,TermPosition)), nl,
	( signature_traits(Name,_T,_V) ->
	  atomic_list_concat(['Signature >',Name,'< already exists'],Message),
	  throw(error(klog_error,
		      (expand_signature/3, Message)))
	;
	  true
	),
	assert(signature_traits(Name,kind,Kind)),
	length(Args,Arity),
	assert(signature_traits(Name,arity,Arity)),
	assert(signature_traits(Name,level,Level)),
	( column_types(Args,ColumnTypes) ->
	  true
	;
	  atomic_list_concat(['Syntax error in args of signature >',Name,'<'],Message),
	  throw(error(klog_error,
		      (expand_signature/3, Message)))
	),
	( has_unique_identifier(ColumnTypes) ->
	  true
	;
	  atomic_list_concat(['repeated self identifier in signature >',Name,'<'],Message),
	  throw(error(klog_error,
		      (expand_signature/3, Message)))
	),
	assert(signature_traits(Name,column_types,ColumnTypes)),
	( column_names_roles(Args,ColumnNamesRoles) ->
	  true
	;
	  atomic_list_concat(['Syntax error in args of signature >',Name,'<'],Message),
	  throw(error(klog_error,
		      (expand_signature/3, Message)))
	),
	( column_names(ColumnNamesRoles,ColumnNames) ->
	  true
	;
	  atomic_list_concat(['Syntax error in args of signature >',Name,'<'],Message),
	  throw(error(klog_error,
		      (expand_signature/3, Message)))
	),
	assert(signature_traits(Name,column_names,ColumnNames)),
	( column_roles(ColumnNamesRoles,ColumnRoles) ->
	  true
	;
	  atomic_list_concat(['Syntax error in args of signature >',Name,'<'],Message),
	  throw(error(klog_error,
		      (expand_signature/3, Message)))
	),
	assert(signature_traits(Name,column_roles,ColumnRoles)),
	% extract_identifiers require that signature trait column type has been asserted already!
	extract_identifiers(Name,ColumnTypes,IDs),
	length(IDs,RelationalArity),
	assert(signature_traits(Name,relational_arity,RelationalArity)),
	assert(signature_traits(Name,ghost,no)).

user:term_expansion((signature S)::Level,(:- true)) :- expand_signature(S,Level).

% ============================================================================


has_unique_identifier(List) :-
	msort(List,SList),
	unique_id(SList).
unique_id([self,self|_Rest]) :- !, fail.
unique_id([_Head|Tail]) :- unique_id(Tail).
unique_id([]).



column_names_roles([],[]).
column_names_roles([NameRole::_Type|Rest],[NameRole|Rest1]) :-
        column_names_roles(Rest,Rest1),!.
column_types([],[]).
column_types([_Name::Type|Rest],[Type|Rest1]) :-
        column_types(Rest,Rest1),!.

column_names([],[]).
column_names([Name@_Role|Rest],[Name|Rest1]) :-
        column_names(Rest,Rest1),!.
column_names([Name|Rest],[Name|Rest1]) :-
        column_names(Rest,Rest1),!.

column_roles([],[]).
column_roles([_Name@Role|Rest],[Role|Rest1]) :-
        column_roles(Rest,Rest1),!.
column_roles([_Name|Rest],[Role|Rest1]) :-
	length(Rest,N),
	atomic_concat(p,N,Role), % Changed the default so that it does not look like an integer - this was creating problems with the graph edge label that needs to contain at least a string.
        column_roles(Rest,Rest1),!.

%% :- dynamic neighborhood/2,neighborhood/3,neighborhood/4,neighborhood/5,neighborhood/6,neighborhood/7.
%% :- dynamic is_neighborhood/1.
:- dynamic current_signature/1.

%% % Neighborhood methods:
%% expand_neighborhood(H,B,NewH,NewB) :-
%% 	current_predicate(current_signature/1), current_signature(CurrentName/Arity),
%%         H=..[neighborhood|Args],
%% 	length(Args,N),
%% 	signature_traits(CurrentName,relational_arity,RA),
%% 	N1 is RA + 1,
%% 	( N is N1 ->
%% 	  true
%% 	;
%% 	  atomic_list_concat(['neighborhood method for signature >',CurrentName,'/',Arity,
%% 			      '<  should have arity ',N1,', not ', N],Message),
%% 	  throw(error(klog_error,
%% 		      (term_expansion((H :- B)), Message)))
%% 	),
%% 	append(Args1,[_],Args), % oops, trick to remove the last element from a list... better ways?
%% 	BaseGoal=..[CurrentName|Args1],
%% 	rappend(BaseGoal,B, NewB),
%% 	NewH=..[neighborhood|[CurrentName|Args]],
%% 	klog_writeln(5,'Here is the neighborhood clause:'),
%%         klog_portray_clause(5,(NewH:-NewB)),
%% 	( is_neighborhood(NewH) ->
%% 	  true
%% 	;
%% 	  assert(is_neighborhood(NewH))
%% 	).
%% user:term_expansion((H :- B),(NewH :- NewB)) :- expand_neighborhood(H,B,NewH,NewB).
%% user:term_expansion(H,(NewH :- NewB)) :- expand_neighborhood(H,true,NewH,NewB).


% Pick methods: (to choose on which tuples of identifiers predictions are requested)
expand_pick(H,B,NewH,B) :-
	current_predicate(current_signature/1), current_signature(CurrentName/Arity),
        H=..[pick|Args],
	length(Args,N),
	signature_traits(CurrentName,relational_arity,RA),
	( N is RA ->
	  true
	;
	  atomic_list_concat(['pick method for signature >',CurrentName,'/',Arity,
			      '<  should have arity ',RA,', not ', N],Message),
	  throw(error(klog_error,
		      (term_expansion((H :- B)), Message)))
	),
        atomic_concat(pick_,CurrentName,PickName),
        NewH=..[PickName|Args],
        %writeln(pick(NewH,B)),
        true.
user:term_expansion((H :- B),(NewH :- NewB)) :- expand_pick(H,B,NewH,NewB).
user:term_expansion(H,(NewH :- NewB)) :- expand_pick(H,true,NewH,NewB).

% Ghost flag
expand_ghost :-
	current_predicate(current_signature/1), current_signature(CurrentName/_),
	retractall(signature_traits(CurrentName,ghost,_)),
	assert(signature_traits(CurrentName,ghost,yes)).
	
user:term_expansion(ghost,(:- true)) :- expand_ghost.

% user:term_expansion((H :- B),(H :- B)) :-
% 	current_predicate(current_signature/1), current_signature(CurrentName/Arity),
% 	write('?? unknown method of signature '), write(CurrentName/Arity), write(': '), write(H),write(' :- '), write(B),nl.

extension_ok(Name) :- signature_traits(Name,level,intensional),signature_traits(Name,ext_clause,_ExtClause).
extension_ok(Name) :- signature_traits(Name,level,extensional).



user:term_expansion(begin_declarations, (:- true)) :-
	%% retractall(domain_traits(_,_)),
        klog_warning('begin_declarations deprecated - use begin_domain').
user:term_expansion(begin_domain, (:- true)) :-
	%% retractall(domain_traits(_,_)),
        %% forall(signature_traits(_S,ext_clause,ExtClause),retractall(ExtClause)),
        %% retractall(signature_traits(_,_,_)),
        true.
% ============================================================================

end_of_domain :-
	reassert_dependency_graph,
        %writeln(end_of_domain),
        setof(S,T^signature_traits(S,column_types,T),Signatures),
	assert(domain_traits(signatures,Signatures)),
        findall(S,T^signature_traits(S,level,extensional),ExtSignatures),
	assert(domain_traits(extensional_signatures,ExtSignatures)),
        findall(S,T^signature_traits(S,level,intensional),IntSignatures),
	assert(domain_traits(intensional_signatures,IntSignatures)),
        ( setof(S,T^(signature_traits(S,column_types,T), memberchk(self,T)),Entities) ->
          assert(domain_traits(entities,Entities))
        ;
	  throw(error(klog_error, (term_expansion((end_domain)), 'There are no entities')))
        ),
        ( setof(S,T^(signature_traits(S,column_types,T), \+memberchk(self,T)),Relationships) ->
          assert(domain_traits(relationships,Relationships))
        ;
	  throw(error(klog_error, (term_expansion((end_domain)), 'There are no relationships')))
        ),
        setof(S,T^(signature_traits(S,column_types,T), signature_traits(S,ghost,no)),ActiveSignatures),
	assert(domain_traits(active_signatures,ActiveSignatures)),
        ( setof(S,T^(signature_traits(S,column_types,T), memberchk(self,T),signature_traits(S,ghost,no)),ActiveEntities) ->
	  assert(domain_traits(active_entities,ActiveEntities))
	;
	  atomic_list_concat(['There are no active entities (all are ghost)!'],Message),
	  throw(error(klog_error,
		      (term_expansion((end_domain)), Message)))
	),
        ( setof(S,T^(signature_traits(S,column_types,T), \+memberchk(self,T),\+signature_traits(S,ghost,yes)),ActiveRelationships) ->
	  assert(domain_traits(active_relationships,ActiveRelationships))
	;
	  atomic_list_concat(['There are no active relationships (all are ghost)!'],Message),
	  throw(error(klog_error,
		      (term_expansion((end_domain)), Message)))
	  ),
	append(Entities,[self,property], Valid),
	forall( member(S,Signatures),
		( signature_traits(S,column_types,ColumnTypes),
		  signature_traits(S,kind,Kind),
		  forall( member(T,ColumnTypes),
			  ( % write(col(S/Signatures,T,Kind,Entities)),nl,
			    ( memberchk(T,Valid) ->
			      %write(T), write('  tested ok'), nl,
			      true
			    ;
			      atomic_list_concat(['Invalid type: >',T,
						  '< - Valid types are entities, `self`, and `property`'],Message),
			      throw(error(klog_error,
					  (term_expansion((end_domain)), Message)))
			    ),
			    ( (Kind = entity, memberchk(T,Entities)) ->
			      atomic_list_concat(['Signature >', S,
						  '< has both self and foreign identifier >',T,'<'],Message),
			      throw(error(klog_error,
					  (term_expansion((end_domain)), Message)))
			    ;
			      true
			    )
			  )
			)
		)
	      ),
	retractall(current_signature(_/_)). % no more current signature

user:term_expansion(end_declarations, (:- true)) :-
    ( end_of_domain -> (true, klog_warning('end_declarations deprecated - use end_domain')) ; 
      throw(error(klog_error, (term_expansion((end_declarations)), "failed to parse kLog script"))) ).

user:term_expansion(end_domain, (:- true)) :-
    ( end_of_domain -> true ; throw(error(klog_error, (term_expansion((end_domain)), "failed to parse kLog script"))) ).

% ============================================================================
% Kurt
% Store all clauses of the executable part of the script for static dependency analysis.
% Uses module reachability.

user:term_expansion(Clause,_Result) :- Clause\=interpretation(_,_), record_reachability(Clause), fail.

% ============================================================================

%user:term_expansion(Clause,Clause) :- default_expansion(Clause).

expand_constructors(Clause,Result) :-
        (Clause = (Head :- _Body) ; Clause = Head),
	Head=..[Name|Args],
	length(Args,NArgs),
        current_signature(Name/NArgs),
        ( signature_traits(Name,level,extensional) ->
          atomic_list_concat(['Extensional signature >',Name,'< cannot have a constructor'],Message),
          throw(error(klog_error, (Clause), Message))
        ;
          true
        ),
        assert(signature_traits(Name,constructor,Clause)),
        ( signature_traits(Name,ext_clause,_ExistingClause) -> % is there one already?
          true
        ;
          length(VarArgs,NArgs),
          ExtClause=..[Name|VarArgs],
          assert(signature_traits(Name,ext_clause,ExtClause))
        ),
        % not working, Yap crashes with tabling...
%        Result = [(:- table Name/NArgs), Clause],
        Result = Clause,
        true.

user:term_expansion(Clause,Result) :- expand_constructors(Clause,Result).

% ============================================================================


% ============================================================================

%% domain_traits(?TraitsSpec,?TraitsVal) is det
%
% Query domain traits. TraitsSpec is one of:
%
% * signatures: list of all signature names
% * entities: list of all entity signature names
% * relationships: list of all relationship signature names

%% domain_traits is det
%
% List on the standard output all the domain traits and signature
% traits for all signatures.

%% domain_traits(+Stream) is det
%
% List on output Stream all the domain traits and signature
% traits for all signatures.

domain_traits :-
        domain_traits(user_output).
domain_traits(Stream) :-
	forall( domain_traits(Key,Val), format(Stream,'~w -> ~w~n',[Key,Val]) ),
	domain_traits(signatures,Signatures),
	forall(member(S,Signatures),
	       ( format(Stream,'Signature: ~w~n',S),
		 forall(signature_traits(S,K,V), portray_trait(Stream,K,V))
	       )
	      ),
        clause(user:kernel_points(KP),_),
        format(Stream,'~w~n',kernel_points(KP)),
	true.

portray_trait(Stream,constructor,V) :-
        format(Stream,'     constructor ->~n',[]),
        format(Stream,'-------------------~n',[]),
        portray_clause(Stream,V),
        format(Stream,'-------------------~n',[]).
portray_trait(Stream,ext_clause,V) :-
        format(Stream,'     ext_clause -> ',[]),
        portray_clause(Stream,V).

portray_trait(Stream,K,V) :-
        format(Stream,'     ~w -> ~w~n',[K,V]).
        

%% signature_traits(?Name,?TraitsSpec,?TraitsVal) is det
%
% Query signature traits. Name is the signature name. TraitsSpec is one of:
%
% * kind: either entity or relationship
% * arity: the number of arguments
% * level: either intensional (deduced) or extensional
% * column_types: list of argument types
% * column_names: list of argument names
% * column_roles: list of argument roles
% * relational_arity: the number of non-property arguments
% * ext_clause: goal for finding all ground tuples for this signature
%
% The value is returned in TraitsVal.

% ============================================================================




%% id_position(+S,-Position) is semidet
%
% If S is the name of a valid entity signature, unify Position with
% the position of the identifier in the argument list. Otherwise fail.

id_position(S,Position) :-
	signature_traits(S,column_types,T),
	nth0(Position,T,self).

%% extract_properties(+Functor,+Args,-Properties) is semidet
%
% If Functor is the name of a valid signature whose arity matches the
% length of the list Args, then unify Properties with the list of
% items in Args that appear at positions of property type. Otherwise
% fail.

extract_properties(Functor,Args,Properties) :-
	signature_traits(Functor,column_types,T),
	ext_prop(Args,T,Properties),!.
extract_properties(Functor,Args,Properties) :-
    throw(error(klog_error, (extract_properties(Functor,Args,Properties),
                             'Failed: maybe the fact below doesnt match its signature?'))).
ext_prop([],[],[]).
ext_prop([A|ARest],[property|TRest],[A|PRest]) :-
	ext_prop(ARest,TRest,PRest).
ext_prop([_|ARest],[T|TRest],PRest) :-
	\+ (T = property),
	ext_prop(ARest,TRest,PRest).

%% extract_identifiers(+Functor,+Args,-IDs) is semidet
%
% If Functor is the name of a valid signature whose arity matches the
% length of the list Args, then unify IDs with the list of items in
% Args that appear at positions of identifier type. Otherwise fail.

extract_identifiers(Functor,Args,IDs) :-
	signature_traits(Functor,column_types,T),
	ext_ids(Args,T,IDs).
ext_ids([],[],[]).
ext_ids([_A|ARest],[property|TRest],PRest) :-
	ext_ids(ARest,TRest,PRest).
ext_ids([A|ARest],[T|TRest],[A|PRest]) :-
	\+ (T = property),
	ext_ids(ARest,TRest,PRest).


%% extract_references(+S,-Refs) is semidet
%
% If s is the name of a valid signature, then unify Refs with the
% subsequence of entity types referenced by S but >self< is replaced
% by S. If a type is referred to multiple times, it appears multiple
% time in Refs.
extract_references(S,Refs) :-
	signature_traits(S,column_types,Types),
	ext_refs(S,Types,Refs).
ext_refs(_,[],[]).
ext_refs(S,[property|TRest],RRest) :-
	ext_refs(S,TRest,RRest).
ext_refs(S,[self|TRest],[S|RRest]) :-
	ext_refs(S,TRest,RRest).
ext_refs(S,[T|TRest],[T|RRest]) :-
	\+ (T = self),
	\+ (T = property),
	ext_refs(S,TRest,RRest).


%% is_kernel_point(+Functor,-YesNo) is semidet
%
% If Functor is the name of a valid signature then unify YesNo with
% 'yes' iff the signature has declared a neighborhood method. All
% graph vertices expanded from this signature will be used as center
% points for the kernel calculation.

%% is_kernel_point(Functor,yes) :-
%% 	is_neighborhood(Head),
%% 	Head=..[neighborhood|[Functor|_Args]].
%% is_kernel_point(_Functor,no).
is_kernel_point(Functor,yes) :-
    user:kernel_points(List),
    member(Functor,List).
is_kernel_point(_Functor,no).
