% From http://rulebench.projects.semwebcentral.org/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% string.pl
%
% String library in Prolog: all strings in Prolog are represented as lists of Ascii codes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(string,[]).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% concatString/3
% concatString(+L1,+L2,-L)
%
% concatenates two strings
% examples: string:concatString("aa","ab",X)
concatString(L1,L2,L):-
  append(L1,L2,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compareString/3
% compareString(+L1,+L2,-R)
%
% returns: the value 0 if the two argument strings L1 and L2 are equal; 
%  a value less than 0 (-1) if the first string L1 is lexicographically less than the second string argument L2; and 
%  a value greater than 0 if the first string is lexicographically greater than the second string argument
% Examples: string:compareString("","",X), string:compareString("a","b",Y), string:compareString("b","a",X), string:compareString("aa","ab",X), string:compareString("","a",X), string:compareString("","",X).
compareString(L1,L2,R):-
  compareString(L1,L2,0,R).
compareString([],[],_Pos,0):-
  !.
compareString([_H|_T],[],_Pos,1):-
  !.
compareString([],[_H|_T],_Pos,-1):-
  !.
compareString([H|T1],[H|T2],Pos,X):-
  !,
  Pos1 is Pos+1,
  compareString(T1,T2,Pos1,X).
compareString([H1|_T1],[H2|_T2],_Pos,1):-
  !,
  H1>H2.
compareString([H1|_T1],[H2|_T2],_Pos,-1):-
  !,
  H1<H2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replaceAll/4
% replaceAll(OldString,Pattern,Replace,NewString).
%
% Examples: replaceAll("a "," ","_",NewString),atom_codes(NewStringAtom,NewString).
replaceAll([],_Pattern,_Replace,[]):- 
  !.
replaceAll(OldString,Pattern,Replace,NewString):-
  startsWith(OldString,Pattern,Rest),
  !,
  replaceAll(Rest,Pattern,Replace,TailNewString),
  append(Replace,TailNewString,NewString).
replaceAll([H|TailOldString],Pattern,Replace,[H|TailNewString]):-
  replaceAll(TailOldString,Pattern,Replace,TailNewString).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% startsWith/3
% startsWith(OldString,Pattern,Rest)
%
% Examples: startsWith(" aaa"," ",Rest)
startsWith(OldString,[],OldString):- 
  !.
startsWith([H|TOldString],[H|T],Rest):- 
  !,
  startsWith(TOldString,T,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% substring/2
% substring(Pattern,Text)
%
% Examples: substring("aaa","bdbsabdsa asd as d dsa sa f as fa sf sa aaa aasfdafd")
substring(Pattern,Text):- 
  startsWith(Text,Pattern,_Rest),
  !.
substring(Pattern,[_H|Text]):- 
  substring(Pattern,Text),
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% toUpperCase/2
% toUpperCase(OldString,NewString).
%
% Examples: toUpperCase("abAc23_ ",NewString),atom_codes(NewStringAtom,NewString).
toUpperCase([],[]):- 
  !.
toUpperCase([H1|T1],[H2|T2]):-
  H1>96,H1<123,
  !,
  H2 is H1-32,
  toUpperCase(T1,T2).
toUpperCase([H1|T1],[H1|T2]):-
  toUpperCase(T1,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% toLowerCase/2
% toLowerCase(OldString,NewString).
%
% Examples: toLowerCase("abAZzc23_ ",NewString),atom_codes(NewStringAtom,NewString).
toLowerCase(L1,L1):-
  var(L1),
  !.
toLowerCase([],[]):- 
  !.
toLowerCase([H1|T1],[H2|T2]):-
  H1>64,H1<91,
  !,
  H2 is H1+32,
  toLowerCase(T1,T2).
toLowerCase([H1|T1],[H1|T2]):-
  toLowerCase(T1,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% capitalized/1
% capitalized(String).
%
% Examples: capitalized("Collins").
capitalized(L1):-
  var(L1),
  !.
capitalized([H1|_T1]):-
  H1>64,H1<91,
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% memberString/2
% memberString(+S,+L)
%
% returns if the string S (in lower case form) is member of the list of strings L (in lower case)
memberString(S,[H|_T]):-
  toLowerCase(S,SL),
  toLowerCase(H,HL),
  SL=HL,
  !.
memberString(S,[_H|T]):-
  memberString(S,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writeString/1
% writeString(+L)
%
% writes a string
% Examples: string:writeString("Lex test")
writeString([]):-
  !.
writeString([H|T]):-
  number(H), % check if is a list of character codes
  atom_codes(A,[H|T]),
  write(A),
  !.
writeString([H|T]):- % sometimes this predicate is called with a list of strings
  is_list(H),
  writeString(H),
  write(' '),
  writeString(T),
  !.
writeString(L):-
  write(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% split/3
% split(+OldString,+Pattern,-ListStrings)
%
% Examples: string:split("a b c"," ",ListStrings),string:writeString(ListStrings).
split(OldString,Pattern,ListStrings):- 
  split(OldString,Pattern,[],ListStrings).
% split(+OldString,+Pattern,+PartialStart,-ListStrings).
split([],_Pattern,[],[]):- 
  !.
split([],_Pattern,PartialStart,[PartialStart]):- 
  !.
split(OldString,Pattern,[],[RestStrings]):-
  startsWith(OldString,Pattern,Rest),
  !,
  split(Rest,Pattern,[],RestStrings).
split(OldString,Pattern,PartialStart,[PartialStart|RestStrings]):-
  startsWith(OldString,Pattern,Rest),
  !,
  split(Rest,Pattern,[],RestStrings).
split([H|T],Pattern,PartialStart,RestStrings):-
  !,
  append(PartialStart,[H],PartialStartTemp),
  split(T,Pattern,PartialStartTemp,RestStrings).

% stringInverse/2
stringInverse(Text,NewText):-
  stringInverse(Text,[],NewText).
% stringInverse/3
stringInverse([],PT,PT):-
  !.
stringInverse([H|T],PT,NT):-
  stringInverse(T,[H|PT],NT).
