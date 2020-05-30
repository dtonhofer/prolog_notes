% ===
% Addition using Peano Numbers
% 
% We represent Peano Numbers not using the recursive s() but using a 
% much more natural way: arrays of 's' with zero mapped to [].
% This is much more natural for the same reason that using character
% arrays is much more natural than using Gödelisation. Fancy that!
%
% https://en.wikipedia.org/wiki/Peano_axioms
%
% In SWI Prolog, set:
%
% set_prolog_flag(answer_write_options,[max_depth(100)]).
% set_prolog_flag(debugger_write_options,[max_depth(100)]).
%
% For full printout
% ===

:- begin_tests(peano).

test(1,true(ExP=[]))          :- user_peanoify(0,ExP,_).
test(2,true)                  :- user_peanoify(N,X,[[key(N),X]]). % translate variable
test(3,true(ExP=[s,s,s]))     :- user_peanoify(3,ExP,_).
test(4)                       :- user_peanoify(3,[s,s,s],_).
test(5,fail)                  :- user_peanoify(2,[s,s,s],_).
test(6,true(ExP=[]))          :- user_peanoify(0,ExP,[]).
test(7,true(ExP=[s]))         :- user_peanoify(1,ExP,[]).
test(8,true(ExP=[s,s]))       :- user_peanoify(2,ExP,[]).
test(9,true(ExP=[s,s,s,s,s])) :- user_peanoify(5,ExP,[]).
test(11,all(Z=[4]))           :- user_peanoify(1+3=Z,quiet).
test(12,all(Z=[1]))           :- user_peanoify(1+0=Z,quiet).
test(13,true)                 :- bagof([X,Y],limit(5,user_peanoify(X+Y=4,quiet)),Bag), Bag = [[0, 4], [1, 3], [2, 2], [3, 1], [4, 0]].
test(15,true)                 :- bagof([X,Y],limit(5,user_peanoify(X+Y=10,quiet)),Bag), Bag = [[0, 10], [1, 9], [2, 8], [3, 7], [4, 6]].
test(16,true)                 :- bagof([X,Z],limit(5,user_peanoify(X+3=Z,quiet)),Bag), Bag = [[0, 3], [1, 4], [2, 5], [3, 6], [4, 7]].
test(17,true)                 :- bagof([X,Z],limit(5,user_peanoify(X+0=Z,quiet)),Bag), Bag = [[0, 0], [1, 1], [2, 2], [3, 3], [4, 4]].
test(18,true)                 :- bagof([Y,Z],limit(5,user_peanoify(3+Y=Z,quiet)),Bag), Bag = [[0, 3], [1, 4], [2, 5], [3, 6], [4, 7]].
test(19,[nondet])             :- bagof([],limit(5,user_peanoify(3+2=5,quiet)),Bag), Bag=[[]].
test(20,true)                 :- bagof([X],limit(5,user_peanoify(X+2=5,quiet)),Bag), Bag = [[3]].
test(21,true)                 :- bagof([Y],limit(5,user_peanoify(3+Y=5,quiet)),Bag), Bag = [[2]].
test(22,true)                 :- bagof([Z],limit(5,user_peanoify(3+2=Z,quiet)),Bag), Bag = [[5]].
test(23,true)                 :- bagof([X,Y],limit(5,user_peanoify(X+Y=3,quiet)),Bag), Bag = [[0, 3], [1, 2], [2, 1], [3, 0]].
test(24,true)                 :- bagof([X,Z],limit(10,user_peanoify(X+2=Z,quiet)),Bag), Bag = [[0,2],[1,3],[2,4],[3,5],[4,6],[5,7],[6,8],[7,9],[8,10],[9,11]].
test(25,true)                 :- bagof([X,Y,Z],limit(10,user_peanoify(X+Y=Z,quiet)),Bag), Bag = [[0,0,0],[0,1,1],[1,0,1],[0,2,2],[1,1,2],[2,0,2],[0,3,3],[1,2,3],[2,1,3],[3,0,3]].
test(26,fail)                 :- user_peanoify(1+5=10,quiet).

:- end_tests(peano).

rt(peano) :- run_tests(peano).

% ===
% Something to replace the ugly var(X),nonvar(X)
% ===

ff(X) :- var(X).     % fresh/unbound variable? 
ii(X) :- nonvar(X).  % instantiated/filled/bound variable?

% ===
% A user test predicate
%
% ?- user_peanoify(12,Ep,L).
% Ep = [s, s, s, s, s, s, s, s, s|...],
% L = [].
%
% ?- user_peanoify(2,Ep,L).
% Ep = [s, s],
% L = [].
%
% ?- user_peanoify(X,Ep,L).
% L = [[key(X), Ep]].
%
% ?- user_peanoify(1+2=3,Ep,L).
% Ep =  ([s]+[s, s]=[s, s, s]),
% L = [].
% ===

user_peanoify(Expr,ExprPeano,Lookup) :-
   peanoify(Expr,ExprPeano,[],Lookup).


% ===
% WORKHORSE PREDICATE
% Call with a sum expression with integers and variables.
% The expression will be refined/verifiedin "Peano Space" so that all the
% variables are bound and true is emitted if the expression is valid.
% Multiple/Infinite solutions can be generated.
%
% ?- bagof(Z,user_peanoify(1+3=Z,quiet),Bag).
% Bag = [4].
%
% ?- bagof([X,Y],user_peanoify(X+Y=4,quiet),Bag).
% Bag = [[0, 4], [1, 3], [2, 2], [3, 1], [4, 0]].
% ===

user_peanoify(Expr,Quiet) :- 
   (ii(Quiet) -> nodebug(peano) ; debug(peano)),
   peanoify(Expr,ExprPeano,[],Lookup),
   ExprPeano=(Xp+Yp=Zp),
   (ii(Quiet) -> true ; 
      (format("Before add : Expr      = ~q\n",[Expr]),
       format("Before add : ExprPeano = ~q\n",[ExprPeano]),
       format("Before add : Lookup    = ~q\n",[Lookup]))),
   peanoadd(Xp,Yp,Zp),
   (ii(Quiet) -> true ; 
      (format("After add  : Expr      = ~q\n",[Expr]),
       format("After add  : ExprPeano = ~q\n",[ExprPeano]),
       format("After add  : Lookup    = ~q\n",[Lookup]))),
   pull(Lookup),
   (ii(Quiet) -> true ; 
      (format("After pull : Expr      = ~q\n",[Expr]),
       format("After pull : ExprPeano = ~q\n",[ExprPeano]),
       format("After pull : Lookup    = ~q\n",[Lookup]))).
   
% ---
% Peano-ify a known integer X ---> [s,s,s,s,s,s,....]
% ---

peanoify(X,Xp,L,L) :- 
   ii(X),integer(X),X>=0,!,
   length(Xp,X),maplist(=(s),Xp).

% ---
% Peano-ify an integer addition expression Prolog freshvars
% ---
                           
peanoify(X,Xp,L,L) :- 
   ff(X),lookup(L,X,PeanoTerm),!,Xp=PeanoTerm.
   
peanoify(X,Xp,Lin,Lout) :- 
   ff(X),\+lookup(Lin,X,_),!,store(X,PeanoTerm,Lin,Lout),Xp=PeanoTerm.

% ---
% Peano-ify an addition expression X+Y=Z ---> Xp+Yp=Zp; this may use or update
% the lookup table, mapping integer addition expression Prolog freshvars to
% Peano addition expression Prolog freshvars from "Lin" to "Lout"
% ---

peanoify(A,B,Lin,Lout)  :- 
   ii(A),A=(X+Y=Z),!,
   peanoify(X,Xp,Lin,L1),
   peanoify(Y,Yp,L1,L2),
   peanoify(Z,Zp,L2,Lout),
   B=(Xp+Yp=Zp).

% ---
% At the end of the Peano expression refinement, go through the lookup and "pull" 
% Peano integers into the Prolog freshvars.
% ---

pull([[_,PeanoTerm]|Ls]) :-
   ff(PeanoTerm),!,pull(Ls).  % Peano Number is still fresh, nothing to pull
pull([[key(A),PeanoTerm]|Ls]) :-
   ii(PeanoTerm),!,length(PeanoTerm,A),pull(Ls). % length/2 counts the numbers of "s"
pull([]).

% ---
% Looking up and storing entries in the "lookup structure" as an integer expression is
% transformed into a Peano expression.
% ---

lookup([[key(A),PeanoTerm]|_ ],X,PeanoTerm)  :- assertion(ff(X)),A==X,!.
lookup([[key(A),_         ]|Ls],X,PeanoTerm) :- assertion(ff(X)),A\=X,!,lookup(Ls,X,PeanoTerm).

store(X,PeanoTerm,Lin,[[key(X),PeanoTerm]|Lin]). % X must not alreay exist as key; PeanoTerm is just a new freshvar

% ---
% Test or backtrackingly-generate Peano numbers.
% Actually used for generation only, we are not interested in testing.
% ---

nat([]).
nat([s|A]) :- nat(A).

% ---
% peanoadd(X,Y,Z) where X,Y,Z are either Peano numbers or freshvars
% shall result in all three variables "validly instantiated" to Peano
% numbers (i.e. the equation is valid according to the axioms for addition
% and there is a single "+" symbol only on the left side at the toplevel-)
%
% The fourth argument does not but is useful when you debug as it says
% what's going on.
% ---

ddd(Text,X,Y,Z) :- catch(ddd_isolate(Text,X,Y,Z),"roll it back",true).

% Try to generate a number out of Peano number

depeanoify_int(X,Xn) :- X==[],!,Xn=[].
depeanoify_int(X,Xn) :- ff(X),!,Xn=X.
depeanoify_int(X,Xn) :- ii(X),X=[s|Xs],ii(Xs),length(X,Xn).
depeanoify_int(X,Xn) :- ii(X),X=[s|Xs],ff(Xs),Xn=1+Xs.

% Print, then throw to make sure any accidental bindings to X,Y,Z
% are rolled back. Prolog is extremely dangerous - you print
% something and then your program no longer works!

ddd_isolate(Text,X,Y,Z) :-
   depeanoify_int(X,Xn),
   depeanoify_int(Y,Yn),
   depeanoify_int(Z,Zn),
   debug(peano,"~s      ~w , ~w , ~w",[Text,Xn,Yn,Zn]),throw("roll it back").

peanoadd(X,Y,Z) :- ii(X),ii(Y),ii(Z), Y=[]     , ddd("i i:0 i",X,Y,Z) , X=Z.
peanoadd(X,Y,Z) :- ii(X),ii(Y),ii(Z), Y=[s|Ys] , ddd("i i:N i",X,Y,Z) , Z=[s|Zs],peanoadd(X,Ys,Zs).
    
peanoadd(X,Y,Z) :- ff(X),ii(Y),ii(Z), Y=[]     , ddd("f i:0 i",X,Y,Z) , X=Z.
peanoadd(X,Y,Z) :- ff(X),ii(Y),ii(Z), Y=[s|Ys] , ddd("f i:N i",X,Y,Z) , Z=[s|Zs],peanoadd(X,Ys,Zs).
    
peanoadd(X,Y,Z) :- ii(X),ff(Y),ii(Z), Y=[]     , ddd("i f~0 i",X,Y,Z) , X=Z.
peanoadd(X,Y,Z) :- ii(X),ff(Y),ii(Z), Y=[s|Ys] , ddd("i f~N i",X,Y,Z) , Z=[s|Zs],peanoadd(X,Ys,Zs).
    
peanoadd(X,Y,Z) :- ii(X),ii(Y),ff(Z), Y=[]     , ddd("i i:0 f",X,Y,Z) , X=Z.
peanoadd(X,Y,Z) :- ii(X),ii(Y),ff(Z), Y=[s|Ys] , ddd("i i:N f",X,Y,Z) , Z=[s|Zs],peanoadd(X,Ys,Zs).

peanoadd(X,Y,Z) :- ff(X),ii(Y),ff(Z), ddd("f i f",X,Y,Z), generate_X_and_test(X,Y,Z).
peanoadd(X,Y,Z) :- ff(X),ff(Y),ii(Z), ddd("f f i",X,Y,Z), generate_X_and_test(X,Y,Z).                            
peanoadd(X,Y,Z) :- ii(X),ff(Y),ff(Z), ddd("i f f",X,Y,Z), nat(Y),peanoadd(X,Y,Z). % guess forever over Y
peanoadd(X,Y,Z) :- ff(X),ff(Y),ff(Z), ddd("f f f",X,Y,Z), nat(Z),peanoadd(X,Y,Z). % guess forever over Z (not over X and Y!)

% nat(X) always succeeds, but peanoadd(X,Y,Z) may not. break off once it does because there are
% no solutions pas that

generate_X_and_test(X,Y,Z) :- nat(X),(peanoadd(X,Y,Z) -> true ; (!,false)).
