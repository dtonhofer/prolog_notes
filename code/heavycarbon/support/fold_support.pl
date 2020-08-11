:- module(heavycarbon_utils_foldsupport,
          [
              all_true/1  % a list contains only "true" atoms
             ,all_false/1 % a list contains only "false" atoms
          ]).

andify_rightwards(true,true,true).
andify_rightwards(true,false,false).
andify_rightwards(false,true,false).
andify_rightwards(false,false,false).

andify_negation_rightwards(true,false,true).
andify_negation_rightwards(true,true,false).
andify_negation_rightwards(false,true,false).
andify_negation_rightwards(false,false,false).


all_true(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_rightwards(FromLeft,E,ToRight)),List,true,Out),
   Out == true.

all_false(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_negation_rightwards(FromLeft,E,ToRight)),List,true,Out),
   Out == true.

