/*
Utils for goals

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

:- module(goals,
	  [rmember/2,
	   rabsent/2,
	   rappend/3,
	   goals_to_list/2,
	   list_to_goals/2]).


/** <module> goals


  Module for manipulating sequences of goals like lists.
  
  @author Paolo Frasconi, taking inspiration from various places

*/



%% rmember(?A,?B) is nondet
%
% True when B is a comma-separated sequence and A occurs in
% it. Similar to member/2 for lists.
rmember(A,A) :- A \= (_,_).
rmember(A, (A,_)).
rmember(A, (_,B)) :- rmember(A,B).

%% rabsent(+A,+B) is det
%
% True when B is a comma-separated sequence and A does not occur in
% it.
rabsent(A,B) :- A \=B,  B \= (_,_).
rabsent(A, (C,D)) :- A \= C, rabsent(A,D).

%% rappend(+A,+B,+C) is det
%
% True if A, B, and C are comma-separated sequences and C is the
% concatenation of A and B.  Useful to generate "negative" edges
% Warning: does not terminate if A is a free variable.
rappend((X,Y),Z,(X,W)) :- rappend(Y,Z,W).
rappend(A,X,(A,X)) :- A \= (_,_).


%% goals_to_list(+G, -L) is det
%
% True if G is a comma-separated sequence, and L the corresponding
% list of items.
% Warning: does not terminate if G is a free variable.
goals_to_list(G, L) :-
        goals_to_list(G, [], L).
goals_to_list((G1, G2), Acc, Res) :-  !,
        goals_to_list(G2, Acc, NAcc),
        goals_to_list(G1, NAcc, Res).
goals_to_list(G, Acc, [G|Acc]).

%% list_to_goals(+L, -G) is det
%
% True if L is a list of items and G the corresponding comma-separated
% sequence Warning: does not terminate if L is a free variable.
list_to_goals([Goal],Goal):- !.
list_to_goals([Goal|Goals],(Goal,Goals1)):- list_to_goals(Goals,Goals1).

