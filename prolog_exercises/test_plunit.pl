:- begin_tests(choicepoints).

test(one)                     :- multi_danger(1).                   % Will warn "Test succeeded with choicepoint"
test(two,[nondet])            :- multi_danger(1).                   % Will not warn, but only collects one
test(three,[all(X == [1,2])]) :- multi_success(X).                  % Collects them all, verifies on the left
test(four)                    :- once(multi_danger(1)).             % Only collects one
test(five)                    :- bagof(X,multi_success(X),[1,2]).   % Collects them all, verifies on the right
test(six,true(Bag == [1,2]))  :- bagof(X,multi_success(X),Bag).     % Collects them all, verifies on the left
test(seven,[fail])            :- multi_success(100).                % Fails at once
test(eight,[throws(_)])       :- bagof(X,multi_danger(X),_Bag).     % Throws in second round
test(nine,[fail])             :- bagof(X,multi_success(X),[_,_,_]). % Fails because it needs three results, but there are two
test(ten,[fail])              :- bagof(X,multi_success(X),[E]).     % Fails because it wants one result, but there are two

multi_danger(X) :- X=1.
multi_danger(_) :- throw("Ouch!").

multi_success(X) :- X=1.
multi_success(X) :- X=2.

:- end_tests(choicepoints).

rt(_) :- run_tests(choicepoints).
