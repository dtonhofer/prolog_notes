
% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% Using a dict to count atomic values encountered during processing:

% ===
% inc(+Key,+DictIn,?DictOut)
% Increment the counter for "Key" in (immutable) dict "DictIn", giving "DictOut".
% If there is not entry for "Key", add a new entry with initial counter value 1.
% This can be used with foldl/N for example.
% ===

inc_for_key(Key,DictIn,DictOut) :- 
   (get_dict(Key,DictIn,X) 
    -> 
    succ(X,XP)
    ;
    XP=1), 
   put_dict(Key,DictIn,XP,DictOut).
   
% ===
% Tests
% ===

:- begin_tests(inc_for_key).

test("just increment for 'a'",true(Do == quux{a:1})) :- 
   Di = quux{}, 
   inc_for_key(a,Di,Do).

test("increment for 'a' twice",true(Do == quux{a:2})) :- 
   Di = quux{},
   inc_for_key(a,Di,D2),
   inc_for_key(a,D2,Do).

test("increment for 'a' and 'b'",true(Do == quux{a:1,b:1})) :- 
   Di = quux{}, 
   inc_for_key(a,Di,D2),
   inc_for_key(b,D2,Do).

:- end_tests(inc_for_key).
