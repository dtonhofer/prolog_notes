% ===
% Probabilistic State Space Search
% From
% https://stackoverflow.com/questions/61550755/block-world-problem-search-runs-out-of-stack-space
% May 2, 2020
% ===

% ===
% Transform a state into a string
% ===

express(state(A,B,C),S) :- 
   express_pos(A,SA),
   express_pos(B,SB),
   express_pos(C,SC),
   atomic_list_concat(["[",SA,",",SB,",",SC,"]"],S).
   
express_pos(on(Top,Rest),S) :- 
   express_pos(Rest,S2), 
   atomic_list_concat([Top,S2],S).
   
express_pos(void,""). 

% ===
% Transform a path into a string
% (The path is given in the reverse order; no matter)
% ===

express_path(Path,PathStr) :-
   express_path_states(Path,StateStrs),
   atomic_list_concat(StateStrs,"<-",PathStr).

express_path_states([S|Ss],[StateStr|SubStateStrs]) :-   
   express_path_states(Ss,SubStateStrs),
   express(S,StateStr).

express_path_states([],[]).

% ===
% For debugging
% ===

debug_proposed(Current,Next,Moved,Path) :-
   express(Current,CurrentStr),
   express(Next,NextStr),
   length(Path,L),
   debug(pather,"...Proposed at path length ~d: ~w -> ~w (~q)",[L,CurrentStr,NextStr,Moved]).

debug_accepted(State) :-
   express(State,StateStr),
   debug(pather,"...Accepted: ~w",[StateStr]).

debug_visited(State) :-
   express(State,StateStr),
   debug(pather,"...Visited: ~w",[StateStr]).

debug_moved(X) :-
   debug(pather,"...Already moved: ~w",[X]).

debug_final(State) :-
   express(State,StateStr),
   debug(pather,"Final state reached: ~w",[StateStr]).

debug_current(State,Path) :-
   express(State,StateStr),
   express_path(Path,PathStr),
   length(Path,L),
   debug(pather,"Now at: ~w with path length ~d and path ~w",[StateStr,L,PathStr]).

debug_path(Path) :-
   express_path(Path,PathStr),
   length(Path,L),
   debug(pather,"Path: ~w (Length ~d)",[PathStr,L]).

% ===
% Moving blocks between three stacks, also recording the move
% ===

move(From,To,Moved) :-
   random_permutation([0,1,2,3,4,5],ONs),  % permute order numbers
   !,                                      % no backtracking past here!
   move_randomly(ONs,From,To,Moved).       % try to match a move 
   
move_randomly([ON|___],From,To,Moved) :- move(ON,From,To,Moved).
move_randomly([__|ONs],From,To,Moved) :- move_randomly(ONs,From,To,Moved).
move_randomly([],_,_,_)               :- debug(pather,"No more moves",[]).

move(0,state(on(X, A), B, C), 
     state(A, on(X, B), C),
     moved(X,"0: A->B")).
     
move(1,state(on(X, A), B, C), 
     state(A, B, on(X, C)),
     moved(X,"1: A->C")).

move(2,state(A, on(X, B), C), 
     state(on(X, A), B, C),
     moved(X,"2: B->A")).
     
move(3,state(A, on(X, B), C), 
     state(A, B, on(X, C)),
     moved(X,"3: B->C")).

move(4,state(A, B, on(X, C)), 
     state(on(X, A), B, C),
     moved(X,"4: C->A")).
     
move(5,state(A, B, on(X, C)), 
     state(A, on(X, B), C),
     moved(X,"5: C->B")).

% ===
% Finding a path from an Initial State I to a Final State F.
% You have to remember the path taken so far to avoid cycles,
% instead of trying to reach the final state while the path-so-far
% is sitting inaccessible on the stack, from whence it can only be
% be reconstructed on return-fro-recursion.
% ===

fail_if_visited(State,Path) :- 
   (memberchk(State,Path) 
   -> (debug_visited(State),fail)
   ; true).

fail_if_moved(moved(X,_),LastMoved) :-
   (LastMoved = moved(X,_)
   -> (debug_moved(X),fail)
   ; true).
   
path2(F,F,Path,Path,_) :- 
    debug_final(F).
    
path2(I,F,PathToI,FullPath,LastMoved) :-
    dif(I,F),                          % I,F are sure different (program will block if it can't be sure)
    debug_current(I,PathToI),
    move(I,Next,Moved),                % backtrackably pattern-match yourself an acceptable next state based on I
    ground(Next),                      % fully ground, btw
    debug_proposed(I,Next,Moved,PathToI),
    fail_if_moved(Moved,LastMoved),    % don't want to move the same thing again
    fail_if_visited(Next,PathToI),     % maybe already visited?
    debug_accepted(Next),              % if we are here, not visited
    PathToNext = [Next|PathToI],
    path2(Next,F,PathToNext,FullPath,Moved). % recurse with path-so-far (in reverse) 

% ---
% Top call
% ---

path(I,F,Path) :- 
   PathToI = [I],
   path2(I,F,PathToI,FullPath,[]),     % FullPath will "fish" the full path out of the depth of the stack
   reverse(FullPath,Path),             % don't care about efficiency of reverse/2 at all
   debug_path(Path).
   
% ===
% Test 
% ===

:- begin_tests(pather).

test(one, true(Path2 = [state(void, void, on(c,on(a,on(b,void)))),
                       state(void, on(c,void), on(void(a,on(b,void)))),
                       state(on(a,void), on(c,void), on(b,void)),
                       state(on(b,on(a,void)), on(c,void), void),
                       state(on(c,on(b,on(a,void))), void, void)]))
                       
     :- I = state(on(c,on(b,on(a,void))), void, void),
        F = state(void, void, on(c,on(a,on(b,void)))),
        path(I,F,Path),reverse(Path,Path2).

:- end_tests(pather).

rt :- debug(pather),run_tests(pather).
