/*
Draw a call graph of klog.

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

:- ensure_loaded(reachability).
:- use_module('klog').
:- write_dependency_graph('callgraph.dot').
:- system("fdp -GK=1 -Goverlap=false -Tpdf callgraph.dot > callgraph.pdf").
:- halt.
