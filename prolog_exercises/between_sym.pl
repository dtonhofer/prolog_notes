% ==
% https://eu.swi-prolog.org/pldoc/doc_for?object=between/3
% 
% A naive symmetric between, which can generate increasing (step 1) and 
% decreasing (step -1) sequences. The "empty sequence", whereby between/3 
% fails at once, no longer exists.
% 
% In the case of a decreasing sequence, this is done by generating the whole
% sequence and storing it. Catastrophic on large sequences! The correct way
% to implement this is via lazy ("generate-on-demand") sequences - just keep
% a small state in an object and compute the next sequence member only when
% requested. In Prolog, the "object" is the stack frame and "when requested"
% is the backtracking "redo". See between/4 further below!
% ==

between_sym(A,B,X) :-

   % No need to check parameters; let between/3 do that
   % - between/3 throws if A,B not integers

   (A =< B)
   ->    
   between(A,B,X)
   ;
   (bagof(Q,between(B,A,Q),Seq),reverse(Seq,SeqR),member(X,SeqR)).
   
:- begin_tests(between_sym).

test(1, all(X = [1,2,3]))   :- between_sym(1,3,X).
test(2, all(X = [3,2,1]))   :- between_sym(3,1,X).
test(1, all(X = [-1,0,+1])) :- between_sym(-1,+1,X).
test(2, all(X = [+1,0,-1])) :- between_sym(+1,-1,X).
test(1, all(X = [0]))       :- between_sym(0,0,X).
test(1, throws(_))          :- between_sym(a,0,_).

:- end_tests(between_sym).

rt(between_sym) :- run_tests(between_sym).
