:- use_module(library(doc_latex)).
%:- [syntax,graphicalize,klog_flags,timing,repair].



run :-
	forall( member(M, [syntax,db,graphicalize,flags,timing,repair]),
		( atomic_concat('../',M,M1),
		  [M1],
		  atomic_concat(M1,'.pl',PrologFile),
		  atomic_concat(M,'.tex',LaTeXFile),
		  do_latex(PrologFile,LaTeXFile)
		)
	      ).


d :-
	doc_latex( graphicalize,
		   'klog.tex',
		   [public_only(false),stand_alone(true)]
		 ).

do_latex(PrologFile, LatexFile):-
	[PrologFile],
        open(LatexFile, write, Stream),
        latex_for_file(PrologFile, Stream, [public_only(false),stand_alone(false)]).

