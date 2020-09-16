:- use_module(enum_domain).
:- use_module(library(ordsets)).

:- debug(enum_domain_test).

%
% ---
%

:- begin_tests(setting_and_getting).

test("retrieve (non-existent) attribute value for nonvar", fail) :-
   % foo evidently appears as bound "X" in enum_domain_get/2
   enum_domain_get(foo,_ATTV).

test("retrieve (non-existent) attribute value from var not carrying the attribute", fail) :-
   enum_domain_get(_X,_ATTV).

test("retrieve attribute value from var carrying the attribute", true(ATTV == [alfa,bravo])) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_get(X,ATTV).

:- end_tests(setting_and_getting).

%
% ---
%

:- begin_tests(setting_and_getting_extras).

test("retrieve attribute value from var carrying the attribute (deconstructing into term)", true([A,B] == [alfa,bravo])) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_get(X,[A,B]).

test("retrieve attribute value from var carrying the attribute (confirming against non-ordset)", true) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_get(X,[bravo,alfa,alfa]).

test("retrieve attribute value from var carrying the attribute (confirming against non-list)", fail) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_get(X,goo).

test("retrieve attribute value from var carrying the attribute (confirming against incorrect list)", fail) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_get(X,[charlie,alfa]).

:- end_tests(setting_and_getting_extras).

%
% ---
%

:- begin_tests(setting_operations).

test("set attribute value, then set again", true(V == [charlie,delta])) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_set(X,[charlie,delta],set),
   enum_domain_get(X,V).

test("set attribute value, then set again, alternative", true(V == [charlie,delta])) :-
   enum_domain_set(X,[alfa,bravo],union),
   enum_domain_set(X,[charlie,delta],set),
   enum_domain_get(X,V).

test("set attribute value, then union with others", true(V == [alfa,bravo,charlie,delta])) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_set(X,[bravo,charlie,delta],union),
   enum_domain_get(X,V).

test("set attribute value, then intersect with others", true(V == [bravo,charlie])) :-
   enum_domain_set(X,[alfa,bravo,charlie],set),
   enum_domain_set(X,[bravo,charlie,delta],intersect),
   enum_domain_get(X,V).

test("set attribute value, then subtract others", true(V == [alfa,bravo])) :-
   enum_domain_set(X,[alfa,bravo,charlie],set),
   enum_domain_set(X,[charlie,delta],subtract),
   enum_domain_get(X,V).

test("set attribute value, then subtract all", fail) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_set(X,[alfa,bravo],subtract).

test("set attribute value, then intersect with empty", fail) :-
   enum_domain_set(X,[alfa,bravo],set),
   enum_domain_set(X,[],intersect).

test("set attribute value to empty", fail) :-
   enum_domain_set(_X,[],set).

test("set attribute value to single value, thus forcing X to take that value", true(X == alfa)) :-
   enum_domain_set(X,[alfa],set).

test("set attribute value, then subtract all but one, forcing X to take that value", true(X == alfa)) :-
   enum_domain_set(X,[alfa,bravo,charlie],set),
   enum_domain_set(X,[bravo,charlie],subtract).

:- end_tests(setting_operations).

%
% ---
%

:- begin_tests(setting_operations_using_hook).

test("set attribute value, then set again with no common values (it's an intersection, and so fails)", fail) :-
   enum_domain_set(X,[alfa,bravo]),
   enum_domain_set(X,[charlie,delta]).

test("set attribute value, then set again with two common values", true(V == [bravo,charlie])) :-
   enum_domain_set(X,[alfa,bravo,charlie]),
   enum_domain_set(X,[bravo,charlie,delta]),
   enum_domain_get(X,V).

test("set attribute value, then set again with one common value, forcing X to take that value", true(X == charlie)) :-
   enum_domain_set(X,[alfa,bravo,charlie]),
   enum_domain_set(X,[charlie,delta]).

:- end_tests(setting_operations_using_hook).

%
% ---
%

:- begin_tests(specific_tests_of_the_lhs_rhs_matrix).

test("LHS: unbound & attributeless, RHS: unbound & attributed (hook not called)", true(I==[])) :-
   reset_hook_call_info,
   enum_domain_set(RHS,[charlie,delta],set),
   LHS=RHS,
   enum_domain_get(LHS,[charlie,delta]),
   get_hook_call_info(I).

test("LHS: unbound & attributed, RHS: unbound & attributeless (hook not called)", true(I==[])) :-
   reset_hook_call_info,
   enum_domain_set(LHS,[charlie,delta],set),
   LHS=_RHS,
   enum_domain_get(LHS,[charlie,delta]),
   get_hook_call_info(I).

% Note that true test uses "=" here as the dict contains vars which don't compare with "=="

test("LHS: unbound & attributed, RHS: unbound & attributed (hook called)", true(I=Expected)) :-
   reset_hook_call_info,
   enum_domain_set(LHS,[bravo,charlie,delta],set),
   enum_domain_set(RHS,[charlie,delta,echo],set),
   LHS=RHS, % hook is called once (and intersects the ordsets)
   enum_domain_get(LHS,[charlie,delta]),
   get_hook_call_info(I),
   Expected=[hooky{
      attv:[charlie,delta,echo],
      attv_of_puv:[bravo,charlie,delta],
      puv:_}].

test("LHS: unbound & attributed, RHS: nonvar (attributeless) (hook called)", true(I==Expected)) :-
   reset_hook_call_info,
   enum_domain_set(LHS,[bravo,charlie,delta],set),
   LHS=charlie, % hook is called and can perform additional checks (here, whether charlie is allowed)
   get_hook_call_info(I),
   Expected=[hooky{
      attv:[bravo,charlie,delta],
      attv_of_puv:none,
      puv:charlie}].

test("LHS: nonvar (attributeless), RHS: unbound & attributed (hook called)", true(I==Expected)) :-
   reset_hook_call_info,
   enum_domain_set(RHS,[bravo,charlie,delta],set),
   charlie=RHS, % hook is called and can perform additional checks (here, whether charlie is allowed)
   get_hook_call_info(I),
   Expected=[hooky{
      attv:[bravo,charlie,delta],
      attv_of_puv:none,
      puv:charlie}].

:- end_tests(specific_tests_of_the_lhs_rhs_matrix).

%
% ---
%

:- begin_tests(perform_various_unifications).

test("LHS & RHS have same attribute values", true(I=Expected)) :-
   reset_hook_call_info,
   enum_domain_set(X,[a,b,c],set),
   enum_domain_set(Y,[a,b,c],set),
   X=Y,
   get_hook_call_info(I),
   Expected=[hooky{
      attv:[a,b,c],
      attv_of_puv:[a,b,c],
      puv:_}].

test("LHS & RHS have differing attribute values, but two values in common", true([I,V]=[Expected,[b,c]])) :-
   reset_hook_call_info,
   enum_domain_set(X,[a,b,c],set),
   enum_domain_set(Y,[b,c,d],set),
   X=Y,
   get_hook_call_info(I),
   Expected=[hooky{
      attv:[b,c,d],
      attv_of_puv:[a,b,c],
      puv:_}],
   enum_domain_get(X,V).

test("LHS & RHS have differing attribute values, and 1 value in common", true([I,X]=[Expected,c])) :-
   reset_hook_call_info,
   enum_domain_set(X,[a,b,c],set),
   enum_domain_set(Y,[c,d,e],set),
   X=Y,
   get_hook_call_info(I),
   Expected=[hooky{
      attv:[c,d,e],
      attv_of_puv:[a,b,c],
      puv:c}].

test("LHS & RHS have differing attribute values, and no value in common", fail) :-
   reset_hook_call_info,
   enum_domain_set(X,[a,b,c],set),
   enum_domain_set(Y,[d,e,f],set),
   X=Y.

:- end_tests(perform_various_unifications).


