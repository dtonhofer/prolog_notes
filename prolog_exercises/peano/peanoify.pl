% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-05-31
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% A possible answer to
%
% https://stackoverflow.com/questions/8954435/convert-peano-number-sn-to-integer-in-prolog
%
% Peano numbers that are actually better adapted to Prolog, in the form of lists.
%
% Why lists?
%
% There is an isomorphism between:
%
% - a list of length N containing only s and terminating in the empty list
% - a recursive linear structure of depth N with function symbols s terminating 
%   in the symbol zero
%    ... so these are the same things (at least in this context).
% - There is no particular reason to hang onto what 19th century mathematicians
%   (i.e Giuseppe Peano ) considered "good structure structure to reason with"
%   (born from function application I imagine).
% - It's been done before: Does anyone actually use Gödelization to encode 
%   strings? No! People use arrays of characters. Fancy that.
% ============================================================================

% ===
% Something to replace (frankly badly named and ugly) "var(X)" and "nonvar(X)"
% ===

ff(X) :- var(X).     % is X a variable referencing a fresh/unbound/uninstantiated term? (is X a "freshvar"?)
bb(X) :- nonvar(X).  % is X a variable referencing an nonfresh/bound/instantiated term? (is X a "boundvar"?)

% ===
% This works if:
% Xn is boundvar and Xp is freshvar: 
%    Map Xn from the domain of integers >=0 to Xp from the domain of lists-of-only-s.
% Xp is boundvar and Xn is freshvar: 
%    Map from the domain of lists-of-only-s to the domain of integers >=0
% Xp is boundvar and Xp is boundvar: 
%    Make sure the two representations are isomorphic to each other (map either
%    way and fail if the mapping gives something else than passed)
% Xp is freshvar and Xp is freshvar: 
%    WE DON'T HANDLE THAT!
%    If you have a freshvar in one domain and the other (these cannot be the same!)
%    you need to set up a constraint between the freshvars (via coroutining?) so that
%    if any of the variables is bound with a value from its respective domain, the
%    other is bound auotmatically with the corresponding value from ITS domain. How to
%    do that? I did it awkwardly using a lookup structure that is passed as 3rd/4th
%    argument, but that's not a solution I would like to see.
% ===
   
peanoify(Xn,Xp) :-
   (bb(Xn) -> integer(Xn),Xn>=0 ; true),                  % make sure Xn is a good value if bound
   (bb(Xp) -> is_list(Xp),maplist(==(s),Xp) ; true),      % make sure Xp is a good value if bound 
   ((ff(Xn),ff(Xp)) -> throw("Not implemented!") ; true), % TODO
   length(Xp,Xn),maplist(=(s),Xp).
  
% ===
% Testing is rewarding! 
% Run with: ?- rt(_).
% ===

:- begin_tests(peano).

test(left0,true(Xp=[]))          :- peanoify(0,Xp).
test(right0,true(Xn=0))          :- peanoify(Xn,[]).
test(left1,true(Xp=[s]))         :- peanoify(1,Xp).
test(right1,true(Xn=1))          :- peanoify(Xn,[s]).
test(left2,true(Xp=[s,s]))       :- peanoify(2,Xp).
test(right2,true(Xn=2))          :- peanoify(Xn,[s,s]).
test(left3,true(Xp=[s,s,s]))     :- peanoify(3,Xp).
test(right3,true(Xn=3))          :- peanoify(Xn,[s,s,s]).
test(f1,fail)                    :- peanoify(-1,_).
test(f2,fail)                    :- peanoify(_,[k]).
test(f3,fail)                    :- peanoify(a,_).
test(f4,fail)                    :- peanoify(_,a).
test(f5,fail)                    :- peanoify([s],_).
test(f6,fail)                    :- peanoify(_,1).
test(bi0)                        :- peanoify(0,[]).
test(bi1)                        :- peanoify(1,[s]).
test(bi2)                        :- peanoify(2,[s,s]).

:- end_tests(peano).

rt(peano) :- run_tests(peano).

