/*
er - draw Entity-Relationship diagrams

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

:- module(er,[ draw_er/1
		 ]).

/** <module> ER

  draw Entity-Relationship diagrams
  
@author Kurt De Grave

*/

:- use_module(library(lists)).
:- use_module('syntax').

draw_er(OutputPrefix) :-
    retract_er_graph,
    domain_traits(signatures,Signatures),
    forall(member(DomainElement,Signatures), assert_er_graph(DomainElement)),
    atomic_concat(OutputPrefix,'-er.dot',OutputGraphFilename),
    open(OutputGraphFilename,write,Stream),
    output_graph(er,Stream,dot),
    retract_er_graph.


% mostly stolen from graphicalize.parse_fact(Ex,Fact,1)
assert_er_graph(DomainElement) :-
	signature_traits(DomainElement,kind,Kind),
	( Kind=entity -> VertexKind=i ; VertexKind=r ),
	signature_traits(DomainElement,column_names,ColumnNames),
	signature_traits(DomainElement,column_roles,ColumnRoles),
	signature_traits(DomainElement,column_types,ColumnTypes),
	assert(er_vertex(DomainElement,DomainElement,VertexKind)),
	forall( nth0(Position,ColumnNames,Name),
		( nth0(Position,ColumnRoles,Role),
		  nth0(Position,ColumnTypes,Type),
		  ( Type = self -> % primary key - do nothing
		    true
		  ;
		    ( signature_traits(Type,kind,entity) ->
				% foreign key
		      %% atomic_list_concat([DomainElement,'$',Role],EdgeLabel),
		      atomic_list_concat([Role],EdgeLabel),
		      assert(er_edge((DomainElement, Type), EdgeLabel))
		    ;		% property
		      flatten([DomainElement,'__',Position],Flat),
		      atomic_list_concat(Flat,AttributeVertex),
		      assert(er_vertex(AttributeVertex,Name,p)),
		      assert(er_edge((DomainElement, AttributeVertex), ''))
		    )
		  )
		)
	      ).


retract_er_graph:-
	nb_setval(last_er_vertex_index,0),
	retractall(er_vertex(_,_,_)),
	retractall(er_edge(_,_)),
	retractall(er_id_2_index(_,_)).


% stolen from graphicalize.pl
output_graph(Ex,Stream,gspan) :-
 	format(Stream,'t # Example: ~w ~n',Ex),
	forall(er_vertex(ID,Attr,VertexKind),
	       ( nb_getval(last_er_vertex_index,Vindex),
		 NewIndex is Vindex+1,
		 nb_setval(last_er_vertex_index,NewIndex),
		 assert(er_id_2_index(ID,Vindex)),
		 ( is_list(Attr) -> list_to_goals(Attr,CSV) ; CSV = Attr ),
		 format(Stream,'v ~w meta:~w,label:~w~n',[Vindex,VertexKind,CSV])
	       )
	      ),
	forall(er_edge((ID1,ID2),Attr),
	       ( er_id_2_index(ID1,Vindex1),
		 er_id_2_index(ID2,Vindex2),
		 format(Stream,'e ~w ~w label:~w~n',[Vindex1,Vindex2,Attr])
	       )
	      ).


% stolen from graphicalize.pl
output_graph(Ex,Stream,gspan) :-
 	format(Stream,'t # Example: ~w ~n',Ex),
	forall(er_vertex(ID,Attr,VertexKind),
	       ( nb_getval(last_er_vertex_index,Vindex),
		 NewIndex is Vindex+1,
		 nb_setval(last_er_vertex_index,NewIndex),
		 assert(er_id_2_index(ID,Vindex)),
		 ( is_list(Attr) -> list_to_goals(Attr,CSV) ; CSV = Attr ),
		 format(Stream,'v ~w meta:~w,label:~w~n',[Vindex,VertexKind,CSV])
	       )
	      ),
	forall(er_edge((ID1,ID2),Attr),
	       ( er_id_2_index(ID1,Vindex1),
		 er_id_2_index(ID2,Vindex2),
		 format(Stream,'e ~w ~w label:~w~n',[Vindex1,Vindex2,Attr])
	       )
	      ).

output_graph(Ex,Stream,dot) :-
    format(Stream,'graph "~w" {~n',Ex),
    forall( er_vertex(ID,Attr,VertexKind),
	    ( nb_getval(last_er_vertex_index,Vindex),
	      NewIndex is Vindex+1,
	      nb_setval(last_er_vertex_index,NewIndex),
	      assert(er_id_2_index(ID,Vindex)),
	      ( is_list(Attr) -> list_to_goals(Attr,CSV) ; CSV = Attr ),
	      format(Stream,'~w [label="~w"',[Vindex,CSV]),
              ( VertexKind='i' -> format(Stream,' shape=box,style=filled,fillcolor=cyan',[]) ; true ),
              ( VertexKind='r' -> format(Stream,' shape=diamond,style=filled,fillcolor=green',[]) ; true ),
              ( VertexKind='p' -> format(Stream,' shape=oval,style=filled,fillcolor=white',[]) ; true ),

              format(Stream,']~n',[])
	    )
          ),
    forall( er_edge((ID1,ID2),Attr),
	    ( er_id_2_index(ID1,Vindex1),
	      er_id_2_index(ID2,Vindex2),
	      format(Stream,'~w -- ~w [label="~w"]~n',[Vindex1,Vindex2,Attr])
	    )
	  ),
    format(Stream,'}~n',[]).

