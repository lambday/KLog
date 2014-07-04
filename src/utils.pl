/*
Misc utilities

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

:- module(utils, [klog_write/2,
                  klog_writeln/2,
                  klog_format/3,
                  klog_format/4,
                  klog_warning/1,
                  kdebug/1,
                  klog_color_format/4,
		  klog_color_format/5,
                  klog_portray_clause/2,
                  aformat/3,
                  new_progress_bar/2,
                  progress_bar/1,
                  range/3,
                  slice/4,
                  shuffle/2]).

:- ensure_loaded(library(lists)).
:- ensure_loaded('flags').

ansictable(black,'[0;30m').
ansictable(red,'[0;31m').
ansictable(green,'[0;32m').
ansictable(yellow,'[0;33m').
ansictable(blue,'[0;34m').
ansictable(magenta,'[0;35m').
ansictable(cyan,'[0;36m').
ansictable(light_gray,'[0;37m').
ansictable(gray,'[1;30m').
ansictable(light_red,'[1;31m').
ansictable(light_green,'[1;32m').
ansictable(light_yellow,'[1;33m').
ansictable(light_blue,'[1;34m').
ansictable(light_magenta,'[1;35m').
ansictable(light_cyan,'[1;36m').
ansictable(white,'[1;37m').

testcolors :- forall(ansictable(Color,Code),format('~w~w[0m~n',[Code,Color])).
% color_format(T,L,C) :-
% 	ansictable(C,Code),
% 	atomic_list_concat([Code,T,'[0m'],T1),
% 	format(T1,L).

% color_format(S,T,L,C) :-
% 	ansictable(C,Code),
% 	atomic_list_concat([Code,T,'[0m'],T1),
% 	format(S,T1,L).


:- set_prolog_flag(single_var_warnings,on).
check(_Module:A,N) :-
    A=..[Name|_],
    length(Args,N), AA=..[Name|Args],
    predicate_property(AA,_Prop),!,
    format(user_error,'[1;35mDid you mean [0m~w?~n',[Name/N]).


undefined(A) :-
    format(user_error,'[1;31mUndefined predicate: [0m~w~n',[A]),
    forall(member(N,[0,1,2,3,4,5,6,7,8,9]),ignore(check(A,N))),
    fail.
:- unknown(_U,undefined(_X)).

user:portray_message(error, error(klog_error,(Context,Message))) :-
        klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        V @>= 1,
	format(user_error,'~n',[]),
	format(user_error,'[1;31mkLog ERROR!! [0m~w~n',[Message]),
	format(user_error,'[1;31min context:  [0m~w~n',[Context]),
	format(user_error,'~n',[]),
	%format(user_error,'[0;33m~w[0m~n',[Formal]),
	true.
user:portray_message(error, error(_Formal,(Context,Message))) :-
	format(user_error,'~n',[]),
	format(user_error,'[1;31mERROR!!      [0m~w~n',[Message]),
	format(user_error,'[1;31min context:  [0m~w~n',[Context]),
	format(user_error,'~n',[]),
	%format(user_error,'[0;33m~w[0m~n',[Formal]),
	true.
:- yap_flag(stack_dump_on_error,on).

user:portray_message(warning, Message) :-
	message_to_string(Message,[Spec-List]),
	format(user_error,'[1;33mWarning!!    [0m',[]),
	format(user_error,Spec,List),
	format(user_error,'~n~n',[]),
	true.

klog_warning(Message) :-
    klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
    V @>= 2,
    format(user_error,'[1;33mWarning!!    [0m',[]),
    format(user_error,"~w",Message),
    format(user_error,'~n~n',[]),
    true.

kdebug(Message) :-
    ( get_klog_flag(klog_debug_messages,yes) ->
      format('[1;35m**** ~w [0m~n',[Message])
    ;
      true
    ).

    
% % by default messages are at the progress level (verbosity=3)
% klog_message(Message) :- klog_message(3,Message).
% klog_message(Verbosity,Message) :-
%         klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
%         ( V @>= Verbosity ->
%           format(user_error,'[1;36mkLog:  [0m~w~n',[Message])
%         ;
%           true
%         ).
klog_write(Verbosity,Term) :-
        klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        ( V @>= Verbosity -> write(Term) ; true).
klog_writeln(Verbosity,Term) :- 
        klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        ( V @>= Verbosity -> writeln(Term) ; true).
% klog_format(Verbosity,Atom,List) :-
%         klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
%         ( V @>= Verbosity -> format(Atom,List) ; true).
klog_format(Stream,Verbosity,Atom,List) :-
        klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        ( V @>= Verbosity -> format(Stream, Atom,List) ; true).
klog_format(Verbosity,Atom,List) :-
        telling(Stream),
        klog_format(Stream,Verbosity,Atom,List).
klog_color_format(Stream,Verbosity,Atom,List,C) :-
        klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        ( V @>= Verbosity ->
          ( ansictable(C,Code),
            format(Stream,Code,[]),
            format(Stream,Atom,List),
            format(Stream,'[0m',[])
          )
        ;
          true
        ).
klog_color_format(Verbosity,Atom,List,C) :-
        telling(Stream),
        klog_color_format(Stream,Verbosity,Atom,List,C).
klog_portray_clause(Verbosity,Clause) :-
        klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        ( V @>= Verbosity -> portray_clause(Clause) ; true).

%% aformat(-Atom,+Format,+List)
%
% similar to sformat but builds an atom instead of a list of char codes.
aformat(Atom,Format,List) :-
        sformat(Codes,Format,List),
        atom_codes(Atom,Codes).
% --------------------------------------------------------------------
:- dynamic bar_status/3.
%% new_progress_bar(+BarName:atom,+MaxCount:integer) is det
%
% create a progress bar called BarName. The gauged task is completed
% when the counter for the bar reaches the integer value MaxCount
new_progress_bar(BarName,MaxCount) :-
        retractall(bar_status(BarName,_,_)),
        assert(bar_status(BarName,1,MaxCount)).
%% progress_bar(+BarName:atom) is det
%
% increase the counter for the progress bar BarName and maybe display
% information on screen.
progress_bar(BarName) :-
        bar_status(BarName,I,MaxCount),
        NumberInterval is max(1,MaxCount // 10),
        DotInterval is max(1,NumberInterval // 10),
	( (I mod NumberInterval) =:= 0 ->
          format('~w',I)
        ;
          ( (I mod DotInterval) =:= 0 -> format('~w','.') ; true)
        ),
	I1 is I+1,
        retract(bar_status(BarName,I,MaxCount)),
        assert(bar_status(BarName,I1,MaxCount)),
	flush_output(user_output).



range(Start,Stop,R):-
        Stop1 is Stop-1,
        crange(Start,Stop1,R).

crange(Start,Stop,R):-crange(Start,Stop,[],R).
crange(Start,Start,R,[Start|R]):-!.
crange(Start,Stop,Acc,R):-Start < Stop, Stop1 is Stop - 1, crange(Start,Stop1,[Stop|Acc],R).

%% slice(?L1,+I,+K,?L2) is det
%
% L2 is the list of the elements of L1 between index I and index K
% (both included).
slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
   K1 is K - 1, slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 1, 
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).

%% shuffle(+Ex:list,-Ex1:list) is det
%
% Unify Ex1 with a random permutation of list Ex.
shuffle(Ex,Ex1) :-
        length(Ex,N),
        random_pick(Ex,N,Ex1).

random_pick(_,0,[]).
random_pick(Pool,N,[Element|MoreElements]) :-
        N > 0,
        length(Pool,L),
        I is random(L) + 1,
        nth1(I,Pool,Element),
        delete(Pool,Element,Pool1),
        N1 is N - 1,
        random_pick(Pool1,N1,MoreElements).
