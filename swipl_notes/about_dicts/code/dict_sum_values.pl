
% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% Sum (over keys) the integer values contained in a dictionary using library(aggregate)
% ===

sum_all_values(_{} ,0) :- !.
sum_all_values(Dict,Total) :- 
   aggregate(sum(V), Key^get_dict(Key,Dict,V), Total).

% ---
% Testing
% ---

:- begin_tests(sum_all_values).

test("Sum over empty dict", true(Total == 0)) :- 
   Dict = quux{},
   sum_all_values(Dict,Total).

test("Sum over nonempty dict", true(Total == 3)) :- 
   Dict = quux{x:1,y:2},
   sum_all_values(Dict,Total).

:- end_tests(sum_all_values).
