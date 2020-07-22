:- module(my_test,[a/2]).
:- use_module(library(chr)).

:- chr_constraint a/2, b/1, c/1.

% ------------
% CHR
% ------------

rule_1 @ a(X,1) <=> in1(X) | b(X) , out1(X).
rule_2 @ a(X,2) <=> in2(X) | c(X) , out2(X).
rule_3 @ b(X) <=> in3(X) | memberchk(X,[1,2]), out3(X).
rule_4 @ c(X) <=> in4(X) | memberchk(X,[u,v]), out4(X).

% ------------
% Prolog called by CHR
% ------------

in1(X) :- format("rule_1 called : a(~q,1) <=> b(~q)\n",[X,X]). 
in2(X) :- format("rule_2 called : a(~q,2) <=> c(~q)\n",[X,X]). 
in3(X) :- format("rule_3 called : b(~q) <=> ~q=1\n",[X,X]). 
in4(X) :- format("rule_4 called : c(~q) <=> ~q=2\n",[X,X]). 

out1(X) :- format("rule_1 success with X=~q\n",[X]). 
out2(X) :- format("rule_2 success with X=~q\n",[X]). 
out3(X) :- format("rule_3 success with X=~q\n",[X]). 
out4(X) :- format("rule_4 success with X=~q\n",[X]). 


