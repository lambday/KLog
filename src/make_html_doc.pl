% First run python doc/ccomm.py C/c_interface.cpp > C/doc_cinterface.pl
% Then run swipl -f make_html_doc.pl
:- use_module(library(doc_files)).
:- [syntax,db,graphicalize,flags,timing,repair,kfold,'C/doc_cinterface.pl'].
:- doc_save(., [recursive(true),doc_root('./html_doc')]).
