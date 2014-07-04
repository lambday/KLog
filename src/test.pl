
:- use_module(library(lists)).
:- use_module(library(maplist)).

test :-
	A=[a,2,t],
	exclude(integer,A,A1),
	write(A1),nl,
	include(integer,A,A2),
	write(A2),nl.