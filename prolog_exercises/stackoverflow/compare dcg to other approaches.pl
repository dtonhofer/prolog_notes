% ===
% Morph DictIn to DictOut so that:
% Only for Keys [a,b,c]:
% If Key exists in DictIn, DictOut is DictIn with the count for Key incremented
% If Key notexists in DictIn, DictOut is DictIn with a new entry Key with count 1
% ===

inc_for_key(Key,DictIn,DictOut) :-
   memberchk(Key,[a,b,c]),
   !,
   add_it(Key,DictIn,DictOut).
   
inc_for_key(Key,DictIn,DictOut) :-
   \+memberchk(Key,[a,b,c]),
   add_it(dropped,DictIn,DictOut).

add_it(Key,DictIn,DictOut) :-
   (get_dict(Key,DictIn,Count) -> succ(Count,CountNew) ; CountNew=1),
   put_dict(Key,DictIn,CountNew,DictOut).

% ===
% Using foldl to count
% ===

count_with_foldl(Atom,DictWithCounts) :-
   atom_chars(Atom,Chars),
   foldl(inc_for_key,Chars,counts{},DictWithCounts).

% ===
% Using a DCG to count
% ===

dcg_count(Dict,Dict)      --> [].
dcg_count(DictIn,DictOut) --> [C], { inc_for_key(C,DictIn,Dict2) }, dcg_count(Dict2,DictOut).

count_with_phrase(Atom,DictWithCounts) :-
   atom_chars(Atom,Chars),
   phrase(dcg_count(counts{},DictWithCounts),Chars).

% ===
% Using standard Prolog to count
% ===

count_with_recursion(Atom,DictWithCounts) :-
   atom_chars(Atom,Chars),
   count_rec(Chars,counts{},DictWithCounts).
   
count_rec([],Dict,Dict).
count_rec([C|Cs],DictIn,DictOut) :- inc_for_key(C,DictIn,Dict2), count_rec(Cs,Dict2,DictOut).

% ===
% Tests
% ===

:- begin_tests(counting).

test("count using foldl/4, #1", true(DictWithCounts == counts{a:3,b:4,dropped:2})) :-
   count_with_foldl(abgdbabba,DictWithCounts).
   
test("count whith phrase/2, #1", [true(DictWithCounts == counts{a:3,b:4,dropped:2}),nondet]) :- 
   count_with_phrase(abgdbabba,DictWithCounts).

test("count whith recursion, #1", [true(DictWithCounts == counts{a:3,b:4,dropped:2})]) :- 
   count_with_recursion(abgdbabba,DictWithCounts).
   
:- end_tests(counting).

