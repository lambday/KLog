/*
Random paritioning of a set of interpretations

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

:- module(random_partition, 
	[	random_partition/3,
		random_partition_stratified/4
	]).

:- ensure_loaded(flags).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(apply_macros)). % maplist

/** <module> random paritioning of a set of interpretations

  An "almost linear" algorithm for random partitioning.

  $ Usage:

  ?- use_module(random_partition).

  ?- random_partition([1, 2, 3, 4], 2, Groups).
  Groups = [[3,1],[4,2]]

  ?- random_partition([1, 2, 3, 4], 2, Groups).
  Groups = [[4,3],[2,1]]
 
  ?- random_partition([1, 2, 3, 4], 3, Groups).
  Groups = [[3],[2],[4,1]]

  $ Algorithm:

  Predicate divide2/5 picks a random bucket number.
  If the bucket is full, then it goes to the right in search of a non-full bucket.
  Predicate random_partition/3 proceeds in 2 steps:
  first, it randomly throws elements in buckets with max capacity #Models/#Buckets
  then the remaining models are thrown onto buckets randomly.

  This module originates from ACE but was significantly modified for kLog.

  $ Results:

  The produced partitions are unordered.

  @author Jan Struyf, Kurt De Grave

*/

add_to_group(_Model, 0, _OrigGroups, [], _Max, _NewGroups) :-
	!,
	fail.
add_to_group(Model, 0, OrigGroups, [(Count, List)|Rest], Max, NewGroups) :-
	!,
	(Count < Max ->
	 Count1 is Count+1,
	 NewGroups = [(Count1, [Model|List])|Rest];
	 add_to_group(Model, 0, OrigGroups, Rest, Max, Rest2),
	 NewGroups = [(Count, List)|Rest2]).
add_to_group(Model, Group, OrigGroups, [X|Groups], Max, [X|NewGroups]) :-
        Group1 is Group-1,
	add_to_group(Model, Group1, OrigGroups, Groups, Max, NewGroups).

divide2([], _, _, Groups, ok(Groups)).
divide2([Model|Rest], NoG, Max, PrevGroups, Result) :-
	random(NoG, Group),
	((add_to_group(Model, Group, PrevGroups, PrevGroups, Max, NewGroups);
	  add_to_group(Model, 0, PrevGroups, PrevGroups, Max, NewGroups)),
	 !,
	 divide2(Rest, NoG, Max, NewGroups, Result)
	 ;
	 Result = nok([Model|Rest], PrevGroups)).

emptygroups(0, []) :- !.
emptygroups(N, [(0,[])|Rest]) :- N1 is N-1, emptygroups(N1, Rest).

remove_counts([], []).
remove_counts([(_,L)|Rest], [L|Rest2]) :- remove_counts(Rest, Rest2).

random(MaxPlusOne,Number) :- Number is random(MaxPlusOne).

% random_partition(+Models:list, +NoG:int, -Groups:list)
%
% An "almost linear" algorithm for random partitioning.
% NoG is the desired number of groups.
random_partition(Models, NoG, Groups) :-
        get_klog_flag(kfold_random_seed,Seed),
        ( Seed = 0 ->
          true
        ;
          srandom(Seed)
        ),  
        length(Models, Ln),
	Max is floor(Ln/NoG), % kdg: changed to floor
	emptygroups(NoG, EGroups),
	divide2(Models, NoG, Max, EGroups, Res),
	(Res = ok(Groups1), !;
	 Res = nok(Overflow, PrelGroups),
	 Max1 is Max+1,
	 divide2(Overflow, NoG, Max1, PrelGroups, ok(Groups1))),
	remove_counts(Groups1, Groups).

% random_partition_stratified(+Models:list, +StratumPred:functor ,+NoG:int, -Groups:list)
%
% StratumPred is a user predicate of arity 2, that takes a model as first parameter
% and returns the model's stratum in the second parameter.
%
% The models belonging to each stratum will be distributed equally over all groups.
% If all of the strata contain less than NoG models, some groups may be empty.
random_partition_stratified(Models, StratumPred, NoG, Groups) :-
	all(S,(member(M,Models),stratum_of_model(StratumPred,M,S)),Strata),
	empty_list_list(NoG, Acc),
        rps_foreach_stratum(Strata, StratumPred, Models, NoG, Acc, Groups).

% Unify L with as list of N empty lists.
empty_list_list(0,[]).
empty_list_list(Size,L) :-
	Size > 0,
	SSize is Size - 1,
	L = [[] | R],
	empty_list_list(SSize,R).

rps_foreach_stratum([], _StratumPred, _Models, _NoG, Acc, Acc).
rps_foreach_stratum([Stratum|R], StratumPred, Models, NoG, Acc, Res) :-
        rps_select_by_stratum(Models, StratumPred, Stratum, SModels),
        random_partition(SModels, NoG, Sets),
        maplist(append, Acc, Sets, NAcc),
        rps_foreach_stratum(R, StratumPred, Models, NoG, NAcc, Res).

rps_select_by_stratum([], _StratumPred, _Stratum, []).
rps_select_by_stratum([Model|R1], StratumPred, Stratum, L2) :-
        (stratum_of_model(StratumPred,Model,Stratum) ->
                L2 = [Model|R2]
        ;
                L2 = R2
        ),
        rps_select_by_stratum(R1, StratumPred, Stratum, R2).

stratum_of_model(StratumPred,Model,Stratum) :-
	StratumGoal =.. [StratumPred,Model,Stratum],
	call(user:StratumGoal).

% debug predicate evenodd(+N, -EvenOrOdd)
%
% test usage of random_partition_stratified/4
%    random_partition_stratified([1,2,3,4], evenodd, 2, Groups).
% should distribute the numbers over Groups of which the sum is always odd, i.e.
%   ensure_loaded(library(lists)).
%   ensure_loaded(library(apply_macros)).
%   random_partition_stratified([1,2,3,4], evenodd, 2, Groups),maplist(sumlist,Groups,Sums),maplist(evenodd,Sums,[odd,odd]).
%
evenodd(M,EO) :-
	Rem is M mod 2,
	(Rem=0 ->
		EO = even
	;
		EO = odd
	).
