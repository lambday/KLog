:- use_module(library(lists)).
:- use_module(klog).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin_domain.
	signature has_position(professor_id::professor, position::property)::extensional.
	signature in_phase(student_id::student, phase::property)::extensional.
	signature years_in_program(p1::student, years::property)::extensional.

	% signature temp_advised_by(p1::person, p2::person)::extensional.
	signature advised_by(p1::student, p2::professor)::extensional.
	signature student(student_id::self)::extensional.
	signature professor(professor_id::self)::extensional.

	%% These are three simple 'features' that may be expected to be
	%% correlated with the target.
	signature on_same_course(s::student,p::professor)::intensional.

	on_same_course(S,P) :-
    	professor(P), student(S), ta(Course,S,Term), taught_by(Course,P,Term).

	signature on_same_500_course(s::student,p::professor)::intensional.
	on_same_500_course(S,P) :-
		professor(P), student(S), ta(Course,S,Term), taught_by(Course,P,Term), course_level(Course,level_500).

	signature on_same_paper(s::student,p::professor)::intensional.
	on_same_paper(S,P) :-
		student(S), professor(P), publication(Pub, S), publication(Pub,P).

	%% Creating complex 'features' is relatively easy in kLog. This one
	%% does not improve accuracy though.

	%% signature n_common_papers(s::student,p::professor,n::property)::intensional.
	%% n_common_papers(S,P,N) :-
	%%         student(S), professor(P),
	%%         setof(Pub, (publication(Pub, S), publication(Pub,P)), CommonPapers),
	%%         length(CommonPapers,N).

	kernel_points([student,professor,on_same_course,on_same_500_course,on_same_paper]).
end_domain.

experiment :-
    klog_flag(klog_master,verbosity,4),
    new_feature_generator(my_fg,rnspdk),
    klog_flag(my_fg,distance,2),
    klog_flag(my_fg,match_type,soft),
    klog_flag(my_fg,radius,2),
    klog_flag(referential_integrity_repair,ignore),
    attach(uwcse_ext),
    new_model(my_model,svm_sgd),
    set_klog_flag(my_model,lambda,0.0001),
    set_klog_flag(my_model,epochs,12),
    set_klog_flag(my_model,lossratio,0.2),
    set_klog_flag(kfold_random_seed,123),
    kfold(advised_by,5,my_model,my_fg),
    true.

libsvm :-
    klog_flag(klog_master,verbosity,4),
    new_feature_generator(my_fg,rnspdk),
    klog_flag(my_fg,distance,2),
    klog_flag(my_fg,match_type,soft),
    klog_flag(my_fg,radius,2),
    klog_flag(referential_integrity_repair,ignore),
    attach(uwcse_ext),
    new_model(my_model,libsvm_c_svc),
    set_klog_flag(my_model,c,20),
    set_klog_flag(my_model,wp,1.0),
    set_klog_flag(my_model,wn,0.2),
    set_klog_flag(kfold_random_seed,123),
    kfold(advised_by,5,my_model,my_fg),
    true.

