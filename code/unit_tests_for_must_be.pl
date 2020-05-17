:- begin_tests(must_be).

% ---
% Old school testing for atom-icity
% Silently fails on fresh variable (instead of throwing).
% ---

test(oldschool_yes)           :- atom(foo).
test(oldschool_no, fail)      :- atom(1/137).
test(oldschool_unknown, fail) :- atom(_).

% ---
% must_be/2: Likes to throw too much.
% Test "must_be_no" had better fail instead of throwing?
% ---

test(must_be_yes) :-
   must_be(atom,foo).

test(must_be_no, error(type_error(_,_),_)) :-
   must_be(atom,1/137).

test(must_be_unknown, error(instantiation_error)) :-
   must_be(atom,_).

% doesn't run backwards either

test(must_be_fwd_only, error(instantiation_error)) :-
   bagof(X, must_be(X,foo), _Types).

% ---
% is_of_type/2: Behaves old-school-ish
% ---

test(is_of_type_yes)           :- is_of_type(atom,foo).
test(is_of_type_no, fail)      :- is_of_type(atom,1/137).
test(is_of_type_unknown, fail) :- is_of_type(atom,_).

% doesn't run backwards

test(is_of_type_fwd_only, error(instantiation_error)) :-
   bagof(X, is_of_type(X,foo), _Types).

:- end_tests(must_be).

rt :- run_tests(must_be).

