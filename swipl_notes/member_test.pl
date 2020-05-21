naive_member(X,[X|_]).
naive_member(X,[_|R]) :- naive_member(X,R).

:- begin_tests(member_determinacy).

% "nondet" suppresses warning "Test succeeded with choicepoint"
% (it is not a test failure if the test succeeds with a choicepoint!)

test(one_nondet)    :- naive_member(1,[1,2,3]).  % generates a warning
test(one,[nondet])    :- naive_member(1,[1,2,3]).
test(two,[nondet])    :- naive_member(1,[1,1,3]).
test(three,[nondet])  :- naive_member(1,[1,1,1]).
test(five,fail)       :- naive_member(3,[1,1,2]).

test(four_a,[nondet]) :- naive_member(2,[1,1,2]).  % nondeterministic on last member
test(four_b)          :- member(2,[1,1,2]).        % deterministic on last member

% Always work, indistinguishable from member

test(one_all_a  ,all(M=[1]))     :- naive_member(M,[1,2,3]),M=1.
test(two_all_a  ,all(M=[1,1]))   :- naive_member(M,[1,1,3]),M=1.
test(three_all_a,all(M=[1,1,1])) :- naive_member(M,[1,1,1]),M=1.
test(four_all_a ,all(M=[2]))     :- naive_member(M,[1,1,2]),M=2.

% Always work

test(one_all_b  ,all(M=[1]))     :- member(M,[1,2,3]),M=1.
test(two_all_b  ,all(M=[1,1]))   :- member(M,[1,1,3]),M=1.
test(three_all_b,all(M=[1,1,1])) :- member(M,[1,1,1]),M=1.
test(four_all_b ,all(M=[2]))     :- member(M,[1,1,2]),M=2.

:- end_tests(member_determinacy).

rt :- run_tests(member_determinacy).


% 1) How can I make the test fail on nondeterminism?
