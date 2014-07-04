/*
kLog database interface

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

:- module(db,
	  [ db_consult/1
	   ]).

/** <module> kLog database interface

  This module provides access to the extensional database. Currently
  it just consults a Prolog database ensuring that facts are loaded
  into the db namespace.

  
  $ Syntactical conventions:

  The extensional declarations should use the special predicate
  db:interpretation/2 whose first argument is an identifier for the
  interpration (any Prolog term) and whose second argument is a fact.


  $ History:

  see git log

@tbd Maybe an interface to a DBMS such as MySQL
  
@author Paolo Frasconi

*/


:- use_module(library(system)).
:- use_module(library(lists)).


zconsult(Filename,Mode) :-
    file_exists(Filename),
    (Mode='-' -> reconsult(Filename) ; consult(Filename)).

zconsult(Filename,Mode) :-
    atom_concat(Filename,'.pl',F1),
    file_exists(F1),
    (Mode='-' -> reconsult(F1) ; consult(F1)).

zconsult(Filename,Mode) :-
    atom_concat(Filename,'.pl.gz',F1),
    file_exists(F1),
    tmpnam(Tmp),
    atomic_list_concat(['gzcat ', Filename, '.pl.gz > ', Tmp],Uncompress),
    system(Uncompress),
    (Mode='-' -> reconsult(Tmp) ; consult(Tmp)),
    delete_file(Tmp).

%% db_consult(+File_s:atom_or_list) is det.
%
% Consult a collection of ground facts from File(s). Also accepts
% basenames of .pl.gz gzipped files.
db_consult(F) :-
    atomic(F),
    zconsult(F,-).

db_consult([H|T]) :-
    zconsult(H,-),
    forall(member(File,T), zconsult(File,+)).

%% db_consult(F) :-
%%         [H|T]=F,
%%         [-H|T]=F1,
%%         F1.
%% db_consult(F) :- reconsult(F).


