/*
k-fold cross-validation

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

:- module(kfold, [ kfold/4,   % Test a custom model and feature generator
                   random_split/4, % Randon train/test split
                   fixed_split/6, % Fixed train/test split
                   prepartitioned_kfold/4, % use a kfold partition file for replicability/comparison against other methods
		   stratified_kfold/5   % Test a custom model and feature generator, using stratified folds.
	         ]).

/** <module> k-fold cross-validation

  This module contains predicates for estimating prediction accuracy
  using k-fold cross validation or a random train/test split. A
  ``fold'' in this context refers to a set of interpretations, several
  cases can be included in a given interpretation. Common testing
  strategies like leave-one-university-out in WebKB or
  leave-one-research-group-out in UW-CSE are naturally obtained by
  setting k to the number of interpretations in the domain. The module
  contains support for stratified k-fold (where strata are specified
  by a user-defined predicate) and prepartitioned k-fold where folds
  are read from file.

  It is recommended that any empirical evaluation of kLog is run using
  predicates in this module since they all save experimental results
  on disk in a structured form (see below).

  $ Usage:

  See predicates kfold/4, random_split/4, fixed_split/6,
  prepartitioned_kfold/4, stratified_kfold/5.

  $ Results:

  Results are saved in a structured way in the file system.  kfold/4
  and random_split/4 create a results directory named after a SHA-1
  hash of: (1) current klog flags, (2) domain traits, (3) train/test
  specification, e.g.
  
  ==
  PWD/results/e789ba9abfa5e1ae77ec0f481dab9971f343df35
  ==

  where PWD is the current working directory. This ensures that
  different experiments run with different parameters (such as kernel
  hyperparameters, regularization, etc) or background knowledge will
  be kept separate and can coexist for subsequent analysis (e.g. model
  selection).

  For k-fold cross-valitation, rhe results directory is structured
  into k sub-directories, one for each fold. These contain the
  following files:

  ==
  auc.log
  hash_key.log      text used to generate the hash key
  output.log        predicted margins for each case in the form target margin # case term
  output.yyy        same after stripping the case term
  output.yyy.pr     recall-precision curve
  output.yyy.roc    ROC curve
  output.yyy.spr    precision points calculated at 100 recall points between 0 and 1.
  test.txt          list of test interpretations in this fold
  train.txt         list of training interpretations in this fold
  ==
  
  @tbd Prediction on attributes (multiclass, regression)

  @credit AUCCalculator by Jesse Davis and Mark Goadrich
  
  @author Kurt De Grave, Paolo Frasconi

*/

% FIXME: in some cases 4-fold and 4 interpretations leaves one fold empty???!!
:- use_module(library(system)).
:- use_module(library(readutil)).
:- use_module(string).

:- ensure_loaded(syntax).
:- ensure_loaded(graphicalize).
:- ensure_loaded(learn).
:- ensure_loaded(flags).
:- ensure_loaded(utils).
:- ensure_loaded(timing).
:- ensure_loaded(random_partition).
:- ensure_loaded(library(lists)).
:- ensure_loaded('C/c_interface').

% The same model can be reused across several runs.
% The declaration of the model is external because the flags decide 
% where the results are saved.

% FIXME: Models vs Model!! if several tasks there are several models. Check that this is OK!
singlefold(Sig, Nr, UnsortedTrain, UnsortedTest, Models, FeatureGenerator,ExpDir) :-
	% For some reason, the kernel library requires sorted example lists
	sort(UnsortedTrain, Train),
	sort(UnsortedTest, Test),
	klog_color_format(3,'Training fold ~w ~n',[Nr],light_yellow),
        %% Make subdirectory for this fold
        aformat(FoldDir,'~w/fold~w',[ExpDir,Nr]),
        aformat(MkFoldDir,'mkdir -p ~w',[FoldDir]),
        system(MkFoldDir),
        prolog_set_model_wd(Models,FoldDir),
        train(Sig,Train,Models,FeatureGenerator),
        %% Save training set performance
        aformat(TrainEvalFilename,'~w/train_eval.log',[FoldDir]),
        report(Models,local),
        report(Models,TrainEvalFilename,local),
	klog_color_format(3,'Trained fold ~w ~n',[Nr],green),
	klog_color_format(3,'Predicting fold ~w ~n',[Nr],light_yellow),
        predict(Sig,Test,Models,FeatureGenerator),
	klog_color_format(3,'Predicted fold ~w ~n',[Nr],green),
        save_predictions(Models,FoldDir,local), % also does AUC
        report(Models,local), % report on screen.. % FIXME: in general a list of models..
        aformat(TrainFilename,'~w/train.txt',[FoldDir]), %
        open(TrainFilename,write,TrainStream),
        format(TrainStream,'~w~n',[Train]),
        close(TrainStream),
        aformat(TestFilename,'~w/test.txt',[FoldDir]),
        open(TestFilename,write,TestStream),
        format(TestStream,'~w~n',[Test]),
        close(TestStream),

        aformat(TargetFilename,'~w/target.txt',[FoldDir]),
        open(TargetFilename,write,TargetStream),
        format(TargetStream,'~w~n',[Sig]),
        close(TargetStream),
        
        %% Save test set performance
        aformat(EvalFilename,'~w/eval.log',[FoldDir]),
        report(Models,EvalFilename,local),
	klog_color_format(3,'Results saved in ~w~n',[FoldDir],light_blue),
        writeln(' '),
        true.

%% stratified_kfold(+Signature, +K, +Models, +FeatureGenerator, +StratumFunctor) is det
%
% Perform a stratified k-fold cross validation. The target Signature
% defines the learning task(s). K is the numnber of folds. Models is a
% (list of) model(s) to be trained and FeatureGenerator defines the
% used kernel. StratumFunctor is the name of a user-declared Prolog
% predicate of the form stratum(+Interpretation,-Stratum); it should
% unify Stratum (e.g. the category for classification) with the
% stratum of the given Interpretation.

stratified_kfold(Signature, N, Models, FeatureGenerator, StratumFunctor) :-
    atomic_list_concat([Signature,', k-fold-CV'],Description),
    prolog_reset_reporter(Models,global,Description),
    make_results_directory(Signature,stratified_kfold,Dir,Made),
    ( Made=false,get_klog_flag(clobber,no) ->
      klog_color_format(2,'Skipping ~w (dir exists)~n',[Dir],light_yellow)
    ;
      ( examples(D),
        ( nonvar(StratumFunctor) ->
          random_partition_stratified(D, StratumFunctor, N, Folds)
        ;
          random_partition(D, N, Folds) % unstratified folds
        )
      ),
      dofolds(Signature, N, Models, FeatureGenerator, Dir, Folds)
    ).

%% prepartitioned_kfold(+Signature, +Models, +FeatureGenerator, +DefinitionFile) is det
%
% Perform a repeated kfold cross-validation reading folds from a file
% containing facts of the form test_int(Trial,Fold,IntId). Each trial
% is saved into a separate subfolder (which in turns contains as many
% folders as folds). This is useful for comparing against other
% methods while keeping the same partitions, so that paired
% statistical tests make sense.

% Caveat: in the biodeg_folds.pl file I have (presumably coming from
% Hendrik) there are both training and test specs, which should be
% redundant since, for each trial, the union of the test sets should
% be the entire data set and in each fold the training set should be
% the difference between the data set and the test set. Here I'm only
% using the test specs.

prepartitioned_kfold(Signature, Models, FeatureGenerator, DefinitionFile) :-
    consult(DefinitionFile),
    make_results_directory(Signature,prepartitioned_kfold,Dir,Made),
    ( Made=false,get_klog_flag(clobber,no) ->
      klog_color_format(2,'Skipping ~w (dir exists)~n',[Dir],light_yellow)
    ;
      ( setof(Trial,FoldNo^Ex^test_int(Trial,FoldNo,Ex),Trials),
        setof(FoldNo,Trial^Ex^test_int(Trial,FoldNo,Ex),FoldNumbers),
        length(FoldNumbers,N),
        forall( member(Trial,Trials),
                ( atomic_list_concat([Signature,', Prepartitioned k-fold-CV'],Description),
                  prolog_reset_reporter(Models,global,Description),
                  bagof(B,F^bagof(Ex,test_int(Trial,F,Ex),B),Folds),
                  aformat(TrialDir,'~w/trial-~w',[Dir,Trial]),
                  dofolds(Signature, N, Models, FeatureGenerator, TrialDir, Folds)
                )
              )
      )),
    true.
    
prolog_reset_reporter(Models,Which,Desc) :-
    flatten(Models,ListOfModels), % so that either model_1 or [model_1,model_2] will be equally valid
    forall(member(Model,ListOfModels), reset_reporter(Model,Which,Desc)).
% Inform models of working directory - needed for external models
prolog_set_model_wd(Models,WD) :-
    flatten(Models,ListOfModels),
    forall(member(Model,ListOfModels), set_model_wd(Model,WD)).

dofolds(Signature, N, Models, FeatureGenerator, Dir, Folds) :-
    forall( between(1,N,FoldNr),
	    ( nth1(FoldNr, Folds, TestData, OtherFolds),
	      append(OtherFolds,TrainData),
	      singlefold(Signature, FoldNr, TrainData, TestData, Models, FeatureGenerator,Dir)
	    )
	  ),
    klog_color_format(3,'~d-fold Cross-validation completed~n',[N],light_blue),
    % klog_format(3,'~n',[]),
    save_predictions(Models,Dir,global),
    aformat(EvalFilename,'~w/eval.log',[Dir]),
    report(Models,EvalFilename,global),
    report(Models,global),
    aformat(TargetFilename,'~w/target.txt',[Dir]),
    open(TargetFilename,write,TargetStream),
    format(TargetStream,'~w~n',[Signature]),
    close(TargetStream),
    save_timings_file(Dir).

%% random_split(+Sig:atom,+Fraction:number,+Models:{atom},+FGenerator:atom) is det
%
% Train and test Model(s) on a given Fraction of existing cases
% (rest used for test). TrainFraction should be either a fraction in
% (0.0,1.0) as a float or in (0,100) as an integer.
random_split(Signature,TrainFraction,Models,FeatureGenerator) :-
    klog_writeln(3,random_split(Signature,TrainFraction,Models,FeatureGenerator)),
    examples(D),
    length(D,N),
    check_fraction(N,TrainFraction,N_Train,TrainFraction100),
    make_results_directory(Signature,random_split,Dir,Made),
    get_klog_flag(kfold_random_seed,Seed),
    ( Seed = 0 -> true ; srandom(Seed) ),  
    ( Made=false,get_klog_flag(clobber,no) ->
      klog_color_format(2,'Skipping ~w (dir exists)~n',[Dir],light_yellow)
    ;
      ( shuffle(D,Shuffled_D),
        N_Train is round(N*TrainFraction100/100.0),
        slice(Shuffled_D,1,N_Train,TrainData),
        N1 is N_Train+1,
        slice(Shuffled_D,N1,N,TestData),
        atomic_concat(train_rnd,TrainFraction100,FoldName),
        singlefold(Signature, FoldName, TrainData, TestData, Models, FeatureGenerator,Dir),
        save_timings_file(Dir)
      )),
    true.

%% fixed_split(+Sig:atom,+Comment:atom,+Train:[atom],+Test:[list],+Models:{atom},+FGenerator:atom) is det
%
% Do train/test on a given fixed split specified by Train and
% Test. Comment can be used to describe the split briefly (will go
% into the results folder name).
fixed_split(Signature,Comment,TrainData,TestData,Models,FeatureGenerator) :-
    klog_writeln(3,fixed_split(Signature,Comment,TrainData,TestData,Models,FeatureGenerator)),
    make_results_directory(Signature,fixed_split,Dir,Made),
    ( Made=false,get_klog_flag(clobber,no) ->
      klog_color_format(2,'Skipping ~w (dir exists)~n',[Dir],light_yellow)
    ;
      ( atomic_concat(train_fixed_,Comment,FoldName),
        singlefold(Signature, FoldName, TrainData, TestData, Models, FeatureGenerator, Dir),
        save_timings_file(Dir)
      )),
    true.


save_timings_file(Dir) :-
        aformat(TimingFilename,'~w/timing.log',[Dir]),
        open(TimingFilename,write,TimingStream),
	report_timings(TimingStream,_),
        close(TimingStream),
        report_timings(_).

model_flags(_Model,[]).
model_flags(Model,[FlagName,FlagValue | R]) :-
	klog_flag(Model,FlagName,FlagValue),
	model_flags(Model,R).

%% kfold(+Signature, +K, +Models, +FeatureGenerator) is det
%
% Perform a k-fold cross validation. Identical to stratified_kfold/5 except no stratum predicate is used.
kfold(Signature, N, Models, FeatureGenerator) :-
	stratified_kfold(Signature, N, Models, FeatureGenerator, _).


%% make_results_directory(-Dir:atom)
%
% Creates a directory for results based on the SHA-1 code of current
% klog flags, e.g.  results/e789ba9abfa5e1ae77ec0f481dab9971f343df35,
% unify Dir with the created directory, and put a log of flags in
% Dir/flags.log
make_results_directory(Signature,EvaluationMode,Dir,Made) :-
        tmpnam(FileName),
        open(FileName,write,Stream),
        format(Stream,'Target signature: ~w~n',Signature),
        format(Stream,'Evaluation mode: ~w~n',EvaluationMode),
        domain_traits(Stream),
        klog_flags(Stream),
        close(Stream),
        getcwd(CurrentDir),
        get_klog_flag(klog_master,install_directory,InstallDir),
        cd(InstallDir),
        aformat(GitCmd, 'git log -1 >> ~w',[FileName]),
        system(GitCmd),
        cd(CurrentDir),
        %aformat(ShasumCmd,'shasum ~w',[FileName]),
        %popen(ShasumCmd,read,ShasumStream),
        %read_line_to_codes(ShasumStream,Codes),
        %string:split(Codes," ",[ShaCodes|_]),
        %aformat(Dir,'results/~s',[ShaCode]),
	shasum(FileName,ShaCode), % defined in c_interface.cpp
	atom_concat(['results/',ShaCode],Dir),
        ( exists_directory(Dir) ->
          Made=false
        ;
          ( aformat(MkDirCmd,'mkdir -p ~w',[Dir]),
            writeln(MkDirCmd),
            system(MkDirCmd),
            Made=true,
            aformat(MvCmd,'mv ~w ~w/hash_key.log',[FileName,Dir]),
            system(MvCmd),
            forall( current(This),
                    ( aformat(CpCmd,'cp ~w ~w/.',[This,Dir]),
                      system(CpCmd),
                      aformat(ZipCmd,'gzip ~w/~w',[Dir,This]),
                      system(ZipCmd)
                    )
                  )
          )
        ).

% save in the results directory also the entire klog script
current(This) :-
    source_file(X),
    atomic_list_concat(Path,'/',X),
    last(Path,This),
    file_exists(This),
    true.

check_fraction(N,Alpha,N1,Alpha) :-
        integer(Alpha),
        Alpha1 is Alpha/100.0,
        check_fraction(N,Alpha1,N1,_).

check_fraction(N,Alpha,N1,Alpha100) :-
        float(Alpha),
        ( ( Alpha < 1.0, Alpha > 0.0) ->
          Alpha100 is round(Alpha*100),
          N1 is round(N*Alpha)
        ;
	  throw(error(klog_error, (random_split(Alpha),
                                   'Invalid train fraction, should be either in (0,1) or in (0,100)')))
        ).

