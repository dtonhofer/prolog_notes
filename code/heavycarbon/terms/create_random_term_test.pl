:- include(library('heavycarbon/terms/create_random_term.pl')).

:- debug(random_term).

:- begin_tests(create_random_term).

/*
test("list") :- 
   node_at_random(list,N,Length),
   debug(random_term,"list: obtained ~q",[N]).

test("open_list") :- 
   node_at_random(open_list,N,Length).
   debug(random_term,"open_list: obtained ~q",[N]).

test("hole") :- 
   node_at_random(hole,N,Length).
   debug(random_term,"hole: obtained ~q",[N]).

test("atom") :- 
   node_at_random(atom,N,Length).
   debug(random_term,"atom: obtained ~q",[N]).

test("compound") :- 
   node_at_random(compound,N,Length).
   debug(random_term,"compound: obtained ~q",[N]).
*/

test("dict of various lengths") :- 
   maplist([Length]>>
      (node_at_random(dict,N,Length),
       debug(random_term,"dict: obtained ~q",[N]))
      [0,1,2]).



   


:- end_tests(create_random_term).
