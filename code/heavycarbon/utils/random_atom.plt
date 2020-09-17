:- use_module(library('heavycarbon/utils/random_atom.pl')).

% This is not really a test, just code to exercise the random_atom
% module. There is no failure criterium.

% :- debug(random_atom).

:- begin_tests(random_atom).

test("run often") :- 
   length(R,1000),
   maplist(random_atom,R),
   debug(random_atom,"At termination: ~q",[R]).

:- end_tests(random_atom).


