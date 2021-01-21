:- use_module(library('heavycarbon/utils/random_atom.pl')).

% This is not really a test, just code to exercise the random_atom
% module. There is no failure criterium.

:- debug(random_atom). % print

:- begin_tests(random_atom).

test("exercising random_char/1") :-
   length(R,100),
   maplist(random_char,R),
   debug(random_atom,"random_char delivers: ~q",[R]).

test("random_text/2, produce atom of length 0",fail) :-
   random_text(_Text,0).

test("random_text/2, produce atom of length 0: if have to set option") :-
   random_text(_Text,0,[allow_empty(yes)]).

test("random_text/2, produce atom of length 0: can be explicitly disallowed",fail) :-
   random_text(_Text,0,[allow_empty(no)]).

test("random_text/3, demand length 10, run 100x") :-
   length(R,100),
   maplist([Text]>>random_text(Text,10),R),
   maplist([Text]>>(assertion((atom(Text),atom_length(Text,10)))),R),
   debug(random_atom,"random atoms of length 10: ~q",[R]).

test("random_text/3, random length, including length 0, run 100x") :-
   length(R,100),
   maplist([Text-Len]>>random_text(Text,Len,[allow_empty(yes)]),R),
   maplist([Text-Len]>>(assertion((atom(Text),atom_length(Text,Len),Len>=0))),R),
   debug(random_atom,"random atoms of random length: ~q",[R]).

test("random_atom/3, random length but not length 0, run 100x") :-
   length(R,100),
   maplist([Text-Len]>>random_text(Text,Len,[allow_empty(no)]),R),
   maplist([Text-Len]>>(assertion((atom(Text),atom_length(Text,Len),Len>0))),R),
   debug(random_atom,"random atoms of random length, but not empty: ~q",[R]).

:- end_tests(random_atom).

