/*
kLog library entry point

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

:- module(klog,
	  [ %%% module graphicalize
	    gl/1,
	    map_prolog_ids_to_vertex_ids/3,
	    attach/0,
	    attach/1,
	    detach/0,
	    check_db/0,
	    examples/1,
	    database/0,
	    database/1,
	    run/1,
	    make_graphs/0,
	    constants/0,
	    assert_example/2,
	    retract_example/1,
	    assert_background_knowledge/1,
	    %%% module syntax
	    domain_traits/0,
	    domain_traits/1,
	    domain_traits/2,
	    signature_traits/3,
	    id_position/2,
	    extract_properties/3,
	    extract_identifiers/3,
	    is_kernel_point/2,
	    op(700,xfx,::),
	    op(500,fx,signature),
	    op(500,fx,predict),
	    op(500,fx,feature),
	    op(500,fx,end),
	    op(200,fy,*),
	    op(500,xfx,@),
	    %%% module repair
	    referential_integrity_repair/1,
            %%% module learn
            train/4,
            predict/4,
            generate_all/3,
            induce_all/2,
            declare_atomic_labels/1,
            save_induced_facts/2,
            %%% module kfold
            tenfold/2,
            kfold/4,
            random_split/4,
            fixed_split/6,
            prepartitioned_kfold/4,
            stratified_kfold/5,
	    %%% module flags
	    set_klog_flag/3,
	    get_klog_flag/3,
	    klog_flag/3,
	    set_klog_flag/2,
	    get_klog_flag/2,
	    klog_flag/2,
	    klog_flags/0,
	    klog_flags/1,
	    %%% module c_interface
	    klog_options/1,
	    add_graph/1,
	    delete_graph/1,
	    write_graph/1,
            export_graph/3,
	    save_as_libsvm_file/1,
	    set_current_graph/1,
	    get_current_graph/1,
	    vertex_alive/2,
	    add_vertex/7,
	    add_edge/5,
	    get_klog_c_flag/3,
	    set_klog_c_flag/3,
            document_klog_c_flags/2,
	    c_klog_flag_client/1,
	    c_klog_flag_client_alt/1,
	    make_sparse_vector/6,
	    write_sparse_vector/1,
	    set_target_label/2,
	    print_graph_ids/0,
            remap_indices/0,
	    new_feature_generator/2,
	    delete_feature_generator/1,
	    new_model/2,
	    delete_model/1,
	    model_type/2,
	    write_model/1,
	    load_model/2,
	    save_model/2,
            check_model_ability/2,
            train_model/2,
            test_dataset/3,
            dot_product/3,
            save_predictions/3,
            get_prediction/3,
            %%% module timing
            report_timings/1,
            report_timings/2,
            %%% module utils
            slice/4,
            shuffle/2
	  ]).


:- ensure_loaded(library(lists)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(random)).

:- ensure_loaded(graphicalize).
:- ensure_loaded(repair).
:- ensure_loaded(learn).
:- ensure_loaded(kfold).
:- ensure_loaded(flags).
:- ensure_loaded(timing).
:- ensure_loaded(utils).
:- ensure_loaded(syntax).
:- ensure_loaded(er).
:- ensure_loaded('C/c_interface').

klog_version('0.1').

:-
        getcwd(D),
        assert(install_directory(D)).
:-
        klog_version(Version),
        format('kLog version ~w~n',[Version]).

% Keeping the source of the user's kLog script is required for automatic dependency analysis.
:- source.
