/*
Create and manipulate flags

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

:- module(flags, [set_klog_flag/3,
		  get_klog_flag/3,
		  klog_flag/3,
                  set_klog_flag/2,
		  get_klog_flag/2,
		  klog_flag/2,
		  klog_flags/0,
                  klog_flags/1]).

:- ensure_loaded(library(lists)).
:- ensure_loaded('C/c_interface').

/** <module> flags

  Module for declaring and manipulating klog flags. For a list of
  supported flags use klog_flags/2.

  $ Developer note: To add more flags, use flag_traits/4 as
  
  ==
  flag_traits(FlagName,C_Or_Prolog,Checker,Description)
  ==
  
  where FlagName is an atom naming the flag, C_Or_Prolog is either the
  atom c or the atom prolog, Checker is a predicate that checks
  whether a value for this flag is legal, and Description is an atom
  describing the flag and its legal values. Prolog flags are directly
  handled in Prolog. C flags are used by the underlying kernel
  calculation system in C++. The declaration mechanism and default
  value assignment is slightly different in the two cases.


@author Paolo Frasconi

*/

:- discontiguous(flag_traits/4).
:- discontiguous(c_klog_flag/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplified interface a la Yap (a bit dangerous if used in tests)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% klog_flag(+FlagName:atom,?Val:atom) is det.
%
% If Val is a free variable, unify it with the current value of the
% flag FlagName. Otherwise sets FlagName to Val.
klog_flag(FlagName,Val) :- ( var(Val) -> get_klog_flag(FlagName,Val) ; set_klog_flag(FlagName,Val) ).

%% klog_flag(+Who:atom,+FlagName:atom,?Val:atom) is semidet.
%
% If Val is a free variable, unify it with the current value of the
% flag FlagName. Otherwise sets FlagName to Val. Who is the handle of
% the kLog object for which the flag is read or set.
klog_flag(Who,FlagName,Val) :- ( var(Val) -> get_klog_flag(Who,FlagName,Val) ; set_klog_flag(Who,FlagName,Val) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% set_klog_flag(+FlagName:atom,+Val:atom) is det.
%
% Sets FlagName to Val.
set_klog_flag(FlagName,Val) :- % Prolog case
	flag_traits(FlagName,_,_,_),
	( check_value(FlagName,Val) ->
	  true
	;
	  atomic_list_concat(['Illegal value >',Val,'< for klog flag >',FlagName,'<'],
			     Message),
	  throw(error(klog_error,
		      (set_klog_flag(FlagName,Val), Message)))
	),
        Old=..[FlagName,_],
        New=..[FlagName,Val],
        retractall(Old),
        assert(New).

check_value(FlagName,Val) :-
	flag_traits(FlagName,_,Checker,_),
	Checker=..[Functor|Args],
	Query=..[Functor|[Val|Args]],
	call(Query).

%% set_klog_flag(+Who:atom,+FlagName:atom,+Val:atom) is det.
%
% Sets FlagName to Val for C++ object Who.
set_klog_flag(Who,FlagName,Val) :- % C++ case
        set_c_klog_flag(Who,FlagName,Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get_klog_flag(+FlagName:atom,-Val) is semidet.
%
% Unify Val with the current value of the flag FlagName.
get_klog_flag(FlagName,Val) :- % Prolog case
	flag_traits(FlagName,_,_,_),
        Query=..[FlagName,Val],
        call(Query).

%% get_klog_flag(+Who:atom,+FlagName:atom,-Val:atom) is det.
%
% Unify Val with the current value of the flag FlagName of C++ object Who.
get_klog_flag(Who,FlagName,Val) :- % C++ case
        c_klog_flag_client(Who),
        get_c_klog_flag(Who,FlagName,Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% klog_flags(+Stream) is det.
%
% Write a description of current flags to Stream.
klog_flags(Stream) :-
	format(Stream,'-----------------------------------------------------------------------------------~n',[]),
	format(Stream,'Prolog-level flags: (current values in square brackets)~n',[]),
	format(Stream,'set_klog_flag(+Flag,+Value) to change a flag~n',[]),
	format(Stream,'get_klog_flag(+Flag,-Value) to get its current value~n',[]),
	format(Stream,'klog_flag(+Flag,?Value) sets or gets a flag value~n',[]),
	format(Stream,'-----------------------------------------------------------------------------------~n',[]),
	forall( flag_traits(FlagName,_CP,Checker,Description),
		describe_flag(Stream,FlagName, Checker, Description)
	      ),
	format(Stream,'-----------------------------------------------------------------------------------~n',[]),
	format(Stream,'Current C++-level flags: (current values in square brakets)~n',[]),
	format(Stream,'[set_|get_]klog_flag(+Who,+Flag,?Value) for flags of a C++ object identified by Who~n',[]),
	format(Stream,'-----------------------------------------------------------------------------------~n',[]),
        forall( c_klog_flag_client(Who),
                ( document_klog_c_flags(Who,Documentation),
                  format(Stream,Documentation,[])
                )
              ),
	format(Stream,'-----------------------------------------------------------------------------------~n',[]).

%% klog_flags is det.
%
% Write a description of current flags to current output stream.
klog_flags :- klog_flags(user_output).

describe_flag(Stream,Flag,Values,Help) :-
        get_klog_flag(Flag,Val),
	format(Stream,'~w ~t ~24+[~w]~n~w~n~w~n~n',[Flag,Val,Values,Help]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Below are the actual flags.
%
% Note: C flags and Prolog flags must be declared differently here!
%       However the difference is invisible outside this module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C Flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% flag_traits(epochs,c,integer,'Number of training epochs for SGD').
% flag_traits(lambda,c,float,'Regularization parameter for SGD').

% flag_traits(radius,c,integer,'Maximum radius for the ball neighborohood').
% flag_traits(distance,c,integer,'Maximum distance between pairs of neighborohoods').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flag_traits(match_type,c,member([hard,soft,hard_soft,multiview]),'>hard< for exact match; >soft< for histogram match; >hard_soft< hybrid >multiview< expand first endpoints').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flag_traits(verbosity,c,member([0,1,2,3,4,5]),'Verbosity level; 0=silent, 1=error, 2=warnings, 3=progress, 4=detailed progress, 5=internal details').
:- set_klog_flag(klog_master,verbosity,2).
:- getcwd(D), set_klog_flag(klog_master,install_directory,D).
:- set_klog_flag(klog_master,save_subgraphs_directory,'').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% int32(I) :-
%         integer(I), I>1, I<32.
% flag_traits(hash_bit_size,c,int32,'Number of bits for the hash function').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic referential_integrity_repair/1.
flag_traits(referential_integrity_repair,
            prolog,member([add,delete,ignore]),
            '>add< the defective E-tuple, >delete< the offending R-tuple, >ignore< to leave a "hanging edge"').
referential_integrity_repair(ignore).

:- dynamic check_duplicate_ids/1.
flag_traits(check_duplicate_ids,
            prolog,member([yes,no]),
            'Set to >yes< to check databases for duplicate IDs').
check_duplicate_ids(no).

:- dynamic klog_debug_messages/1.
flag_traits(klog_debug_messages,
            prolog,member([yes,no]),
            'Turns on additional debug messages').
klog_debug_messages(yes).

:- dynamic kfold_random_seed/1.
flag_traits(kfold_random_seed,
            prolog,integer,
            'Seed for the k-fold random generator. Zero means use current state').
kfold_random_seed(0).

:- dynamic pthreshold/1.
flag_traits(pthreshold,
            prolog,float,
            'If margin > pthreshold then case is induced (predicted as positive)').
pthreshold(0.0).

:- dynamic clobber/1.
flag_traits(clobber,
            prolog,member([yes,no]),
            'If set to no, prevents result directories from being overwritten').
clobber(no).

:- dynamic n_debug_graphs/1.
flag_traits(n_debug_graphs,
            prolog,integer,
            'Number of graphs to be dumped for debugging purposes').
n_debug_graphs(5).
