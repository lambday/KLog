/*
Simple profiling

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

/** <module> timing


  Very simple profiling module
  
@author This stuff is shamelessly stolen from ACE.

*/

:- module(timing, [init_timings/0,
		   use_timer/2,
		   use_timer/3,
		   report_timings/1,
		   report_timings/2]).

:- ensure_loaded('flags').

:- dynamic recorded_time/2.

init_timings :-
	retractall(recorded_time(_,_)).


:- meta_predicate(time(:,-)).
time(C, T) :-
	statistics(runtime,[T1,_]),
	% statistics(garbage_collection, [_,_,GC1]),
	% statistics(stack_shifts, [_,_,S1]),
	C,
	statistics(runtime,[T2,_]),
	% statistics(garbage_collection, [_,_,GC2]),
	% statistics(stack_shifts, [_,_,S2]),
  %write(runtime(T2-T1)),nl,
  %write(gc(GC2-GC1)),nl,
  %write(ss(S2-S1)),nl,
	% T is (T2-T1+S2-S1+GC2-GC1)/1000.0.
	T is (T2-T1)/1000.0.
% garbage_collection & stack_shifts were problematic with swipl

:- meta_predicate use_timer(+,:).

use_timer(C, X, T) :-
	time((X->Suc=1;Suc=0), T),
	(recorded_time(C, Time) ->
 	 retract(recorded_time(C, Time));
	 Time = 0),
	NewTime is T+Time,
	asserta(recorded_time(C, NewTime)),!,
	Suc=1.
use_timer(C,X) :-
	use_timer(C,X,_).


report_timings(Total) :-
	telling(F),
	report_timings(F,Total),
        get_klog_flag(klog_master,verbosity,VF),atom_number(VF,V),
        ( V>4 ->
          format(F,'~nOverall statistics:~n',[]),
          statistics
        ;
          true
        ).

report_timings(F,Total) :-
	retractall(total(_)),
	assert(total(0)),
	forall( recorded_time(C, Time),
		( format(F,"Time on timer ~w:~42+~t~3f~8+~n",[C,Time]),
		  total(PreviousTotal),
		  Total is PreviousTotal + Time,
		  retractall(total(_)),
		  assert(total(Total))
		)
	      ),
	total(Total),
	format(F,"~w:~42+~t~3f~8+~n",['Total timed',Total]),
        statistics(runtime,[RunTime,_]),
        statistics(cputime,[CpuTime,_]),
        statistics(walltime,[WallTime,_]),
        statistics(heap,[Used,Free]),
        format(F,'~n',[]),
        SecRunTime is RunTime/1000.0,
	format(F,"~w:~42+~t~3f~8+~n",['Total run time',SecRunTime]),
        SecCpuTime is CpuTime/1000.0,
	format(F,"~w:~42+~t~3f~8+~n",['Total CPU time',SecCpuTime]),
        SecWallTime is WallTime/1000.0,
	format(F,"~w:~42+~t~3f~8+~n",['Total wall time',SecWallTime]),
        MBUsed is (Used/1024)/1024,
	format(F,"~w:~42+~t~3f~8+~n",['Used heap (MB)',MBUsed]),
        MBFree is (Free/1024)/1024,
	format(F,"~w:~42+~t~3f~8+~n",['Free heap (MB)',MBFree]).

