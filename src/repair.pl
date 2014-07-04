/*
Check and repair referential integrity constraints

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

:- module(repair,[ check_and_repair/1
		 ]).

:- ensure_loaded(library(lists)).
:- ensure_loaded('flags').
:- ensure_loaded('graphicalize').
:- ensure_loaded('timing').
:- ensure_loaded('goals').
:- ensure_loaded('utils').
:- ensure_loaded('syntax').

/** <module> Referential integrity constraints

  A referential integrity constraint violation occurs if an R-tuple
refers to an undefined identifier (i.e. there is no E-tuple identified
by this foreing key). Violations can be repaired or ignored.

  @see =klog_flag= =referential_integrity_repair=
*/

%% check_and_repair(+Ex:atom) is det
%
% Scan the database for possible referential integrity violations and
% repair them. The repair strategy is defined by the flag
% =referential_integrity_repair=. If the value is =add=, then the
% defective E-tuple is added. If the value is =delete= then the
% offending R-tuple is deleted. If the value is =ignore= an exception
% is raised: simply don't call this predicate if no check should be
% performed.

check_and_repair(Ex) :-
        %klog_writeln(5,check_and_repair(Ex)),
	forall(db:interpretation(Ex,Fact), 
	       ( check_and_repair(Ex, Fact) ->
		 true
	       ;
                 throw(error(klog_error,
                             (check_and_repair(Ex,Fact), 'check_and_repair failed')))
	       )
	      ).

check_and_repair(Ex,Fact) :-
        Fact=..[S|Args],
        signature_traits(S,column_types,Column_Types),
        forall( nth0(Position,Column_Types,Type),               
                ( member(Type,[property,self]) -> true
                ;
                  ( nth0(Position,Args,ID), % ID is a reference, check it
                    % retrieve colname of ID in its entity signature
                    signature_traits(Type,column_types,TColumn_Types),
                    nth0(N,TColumn_Types,self),
                    graphicalize:domain_identifiers(Ex,Type,Constants),
                    ( member(ID,Constants) ->
                      true
                      ;
                      repair(Ex,Fact,ID,N,TColumn_Types,Type)
                    ),
                    true
                  ),
                 true
                )
              ),
        true.
check_and_repair(_Ex,Fact) :-
	Fact=..[Functor|_Args],
	\+ signature_traits(Functor,kind,_).

% First strategy, add defective E-tuple
repair(Ex,Fact,ID,N,TColumn_Types,Type) :-
        get_klog_flag(referential_integrity_repair,add),
        klog_format(5,'integrity repair (add): ~w referenced in ~w does not exist in ~w~n', [ID,Fact,Ex]),
        %%signature_traits(S,column_types,ColumnTypes),
        %writeln(signature_traits(S,column_types,ColumnTypes)),
        % create list of args for filling in with ?
        length(TColumn_Types,NArgs),
        length(NewFactArgs,NArgs),
        nth0(N,NewFactArgs,ID),
        fill_with_question_marks(NewFactArgs,NewFactArgs1),
        %writeln(fill_with_question_marks(NewFactArgs,NewFactArgs1)),
        NewFact=..[Type|NewFactArgs1],
        writeln(NewFact=..[Type|NewFactArgs1]),
        ( db:interpretation(Ex,NewFact) ->
          klog_color_format(4,'?? fixed already as ~w ~n', [NewFact],red),
          true
        ;
          klog_color_format(4,'fixing ICV: assert ~w ~n', [NewFact],green),
          assert(db:interpretation(Ex,NewFact))
        ),
        true.

% Second strategy, delete offending R-tuple
repair(Ex,Fact,ID,_,_,_) :-
        get_klog_flag(referential_integrity_repair,delete),
        klog_format(5,'integrity repair (delete): ~w referenced in ~w does not exist in ~w~n', [ID,Fact,Ex]),
        retract(db:interpretation(Ex,Fact)).
% caveat: there is a funny bug in OmniGraffle, when using this
% strategy with the fake cornell_ext, some vertices are not shown
% correctly but are there (use graphviz to see).

% otherwise the flag is ignore (third stragegy) but we should not be here 
repair(Ex,Fact,_,_,_,_,_) :-
        throw(error(klog_error, (repair(Ex,Fact), 'Why am I here if referential integrity is ignore???'))).

fill_with_question_marks([],[]).
fill_with_question_marks([Head|Tail],[NewHead|NewTail]) :-
	( var(Head) ->
	  NewHead = ?
	;
	  NewHead = Head
	),
	fill_with_question_marks(Tail,NewTail).
	

