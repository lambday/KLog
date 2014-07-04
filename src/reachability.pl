/*
Predicate dependency graph

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	author		Tom Schrijvers, Kurt De Grave
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(reachability,[write_dependency_graph/2,  % for simple prolog code only
			write_dependency_graph/1,  % for kLog, requires prior use of record_reachability/1 and reassert_dependency_graph/0
			record_reachability/1,
			reassert_dependency_graph/0,
			depends_directly/2   %requires prior use of record_reachability/1 and reassert_dependency_graph/0
			%depends_transitive/2  %requires prior use of record_reachability/1 and reassert_dependency_graph/0
			]). 

:- dynamic depends_directly/2.

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(apply_macros)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read a Prolog file and write a file containing the dependency graph.
% This works only on files with normal Prolog syntax, not on kLog scripts.
%
write_dependency_graph(In,Out) :-
	readfile(In,Declarations),
	call_pairs(Declarations,UPairs,[]),
	sort(UPairs,Pairs), % remove duplicates
	write_dot_file(Out,Pairs).

% Write a graph to file Out, representing the program that was stored using record_reachability/1.
% 
write_dependency_graph(Out) :-
	findall(T,recorded(depanalysis,T,_Ref),Clauses),
	call_pairs(Clauses,UPairs,[]),
	sort(UPairs,Pairs), % remove duplicates
	write_dot_file(Out,Pairs).

% Store a clause of a program so we can construct the dependency graph of the program.
%
% Recommended use is with term_expansion, e.g.:
%    user:term_expansion(T,T) :- record_reachability(T).
%
record_reachability(T) :-
	( T = (_ :- _) ->
		T = Clause
	;
		Clause = (T :- true)
	),
	recordz(depanalysis,Clause,_Ref).

reassert_dependency_graph :-
	retractall(depends_directly(_U, _V)),
	findall(T,recorded(depanalysis,T,_Ref),Clauses),
	call_pairs(Clauses,UPairs,[]),
	!,
	forall((member(Pair, UPairs),Pair = (Source - Target)),
		(\+ depends_directly(Source,Target) ->
			assert(depends_directly(Source,Target))
		;
			true
		)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File Reading

readfile(File,Declarations) :-
	see(File),
	readcontent(Declarations),
	seen.

readcontent(C) :-
	read(Term0),
	expand_term(Term0,Term),
	( Term == end_of_file ->
		C = []
	; Term = (:- Decl) ->
		( Decl = op(X,Y,Z) ->
			op(X,Y,Z)
		;
			true
		),
		readcontent(C)
	; 
		( Term = (_ :- _) ->
			Term = Clause
		;
			Clause = (Term :- true)
		),
		C = [Clause | Terms],
		readcontent(Terms)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%


call_pairs([]) --> [].
call_pairs([C|Cs]) -->
	call_pairs_clause(C),
	call_pairs(Cs). 

call_pairs_clause((Head :- Body)) --> 
	{ functor(Head,F,A) },
	call_pairs_body(Body,F/A).

call_pairs_body(B,_Start) --> { var(B) }, !.
call_pairs_body(true,_) --> [].
call_pairs_body((G1,G2),Start) --> !,
	call_pairs_body(G1,Start),
	call_pairs_body(G2,Start).
call_pairs_body((G1;G2),Start) --> !,
	call_pairs_body(G1,Start),
	call_pairs_body(G2,Start).
call_pairs_body((G1->G2),Start) --> !,
	call_pairs_body(G1,Start),
	call_pairs_body(G2,Start).
call_pairs_body(maplist(G,_),Start) --> !,
	[ Start - maplist/2 ],
	( { nonvar(G) } ->
		{ functor(G,F,A), A1 is A + 1 },
		[ Start - F/A1 ]
	;
		[]
	).

call_pairs_body(G,Start) -->
	{ 	predicate_property(G,meta_predicate(M)),
		G =.. [Functor|Args],
		M =.. [Functor|Metanesses]
	},
	call_pairs_metalist(Args,Metanesses,Start).

call_pairs_body(G,Start) -->
	( { 	\+ predicate_property(G,built_in),
		functor(G,F,A) } ->
		[ Start - F/A ]
	;
		[]
	).


call_pairs_metalist([],[],_Start) --> [].
call_pairs_metalist([G|RG],[Metaness|RMeta],Start) -->
	( {	(
			Metaness = :
		;
			integer(Metaness)
		)
	} ->
		call_pairs_body(G,Start)
	;
		[]
	),
	call_pairs_metalist(RG,RMeta,Start).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Create dot file

write_dot_file(Out,Pairs) :-
	open(Out,write,Stream,[]),
	with_output_to(Stream,write_dot_file_(Pairs)),
	close(Stream).

write_dot_file_(Pairs) :-
	writeln('digraph {'),
	maplist(write_dot_pair,Pairs),
	writeln('}').

write_dot_pair(X-Y) :- format('  "~w" -> "~w";\n',[X,Y]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
