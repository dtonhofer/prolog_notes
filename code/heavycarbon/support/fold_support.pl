:- module(heavycarbon_utils_foldsupport,
          [
              all_true/1  % a list contains only "true" atoms
             ,all_false/1 % a list contains only "false" atoms
          ]).

% success means "I can compute this" (and the truth value is in arg 3)
% whereas failure means "I can'compute this"

% andify(Element,FromLeft,ToRight)

and_ify(true,true,true)    :- !. % Found "true" and the AND is still "true" --> keep "true"
and_ify(true,false,false)  :- !. % Exists to allow actually checking the list elements
and_ify(false,true,false)  :- !. % Exists to allow actually checking the list elements
and_ify(false,false,false) :- !. % Exists to allow actually checking the list elements

% and_not_ify(Element,FromLeft,ToRight)

and_not_ify(false,true,true)   :- !. % Found "false" and the AND is still "true" --> keep "true"
and_not_ify(false,false,false) :- !. % Exists to allow actually checking the list elements
and_not_ify(true,false,false)  :- !. % Exists to allow actually checking the list elements 
and_not_ify(true,true,false)   :- !. % Exists to allow actually checking the list elements

all_true(List)  :- foldl(and_ify,List,true,true). % succeeds if the outcome is "true"
all_false(List) :- foldl(and_not_ify,List,true,true). % succeeds if the outcome is "true"

