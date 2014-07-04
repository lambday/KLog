%% @author Paolo Frasconi <p-f@dsi.unifi.it>
%% @brief load shared library into Prolog

%% Copyright (C) 2013-  Fabrizio Costa, Kurt De Grave, Luc De Raedt, Paolo Frasconi

%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.

%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

:- expects_dialect(yap).
:- load_foreign_files(['c_interface'],[],'c_init').

test :-
	add_graph(g1),
	add_graph(g2),
	add_graph(g3),
	add_graph(g4),
%	set_current_graph(g1),
%	get_current_graph(G), write('Current graph is '), write(G), nl,
	% delete_graph(g1),
	% get_current_graph(G), write('Current graph is '), write(G), nl,
	add_vertex(g1,[a,b,2,4.4],yes,no,r,V1),
	add_vertex(g1,[c,d,1.2,4],no,no,i,V2),
	add_vertex(g1,[f,g,5.42,7],no,yes,r,V3),
%	turn_vertex_on(V3),
%	turn_vertex_off(V3),
	write(v(V1,V2,V3)),nl,
	add_edge(g1,V1,V3,[s,t],_EdgeId),
	add_edge(g1,V1,V2,[s,t],_EdgeId2),
	add_edge(g1,V2,V3,[s,t],_EdgeId3).


/*
Now try the goal
   cleanup_data,test,cleanup_data,halt.
in a valgrind YAP:
   echo "cleanup_data,test,cleanup_data,halt." | valgrind --tool=memcheck --leak-check=yes yap -l c_interface.pl
or when the cc source has changed:
make -j 2; echo "cleanup_data,test,cleanup_data,halt." | valgrind --tool=memcheck --leak-check=yes yap -l c_interface.pl

*/
