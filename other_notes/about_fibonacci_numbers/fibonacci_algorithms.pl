:- module(fibonacci_algorithms,
          [
           fib/2
          ,fib_bottomup_direct/2
          ,fib_bottomup_direct_usv/4
          ,fib_bottomup_dict_cache/3
          ,fib_bottomup_frozen_cache/3
          ,fib_bottomup_lazylist_cache/3
          ,fib_matrixmult/2
          ,fib_matrixmult_usv/4
          ,fib_matrixmult_streamlined/2
          ,fib_matrixmult_streamlined_usv/4
          ,fib_fast_doubling/2
          ,fib_topdown_list_cache_ascending/3
          ,fib_topdown_list_cache_descending/3
          ,fib_topdown_list_cache_descending_debug/3
          ,fib_topdown_list_cache_descending_cautious/3 
          ,fib_topdown_list_cache_clpfd/3
          ,fib_topdown_dict_cache/3
          ,fib_bottomup_lazylist_cache/3
          ,retrieve/3
          ,matrixpow/3
          ,matrixpow_streamlined/3
          ,fib_golden_ratio/2
          ]).
 
:- use_module(library(clpfd)).

/*  Zero-Clause BSD (0BSD) follows (https://opensource.org/licenses/0BSD)

    Permission to use, copy, modify, and/or distribute this software for
    any purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
    WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
    AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
    DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
    TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
*/

% =============================================================================
% Compute the Fibonacci Series, F = fib(N), for N >= 0, using various
% approaches.
%
% More on this series:
% https://en.wikipedia.org/wiki/Fibonacci_number
%
% The idea for the lazy list comes from
% http://rosettacode.org/wiki/Fibonacci_sequence#Prolog
% 
% TODO:
% 
% - Add a CHR example
% - How to collect performance info and graph it, in repeatable fashion?
% - Add "double fibonacci" from 
%     https://muthu.co/fast-nth-fibonacci-number-algorithm/
% - Add "linear algebra fibonacci" which uses the eigenvectors of the 
%   [[1 1][1 0]] transformation matrix to get a closed formula
%     https://stackoverflow.com/questions/38445069/fast-fibonacci-computation/41771104#41771104
%     https://stackoverflow.com/questions/67972830/prolog-finding-the-nth-fibonacci-number-using-accumulators/
% =============================================================================

% Carve the constants fib(0) and fib(1) out of the code.

const(fib0,0).
const(fib1,1).

% -----------------------------------------------------------------------------
% Naive implementation, but "tabled" (i.e. Prolog caches the call results)
%
% ?- fib(10,F).
% F = 55.
% -----------------------------------------------------------------------------

:- table fib/2.

fib(N,F) :-
   N>1,
   !,
   NA is N-1, fib(NA,FA),
   NB is N-2, fib(NB,FB),
   F is FA + FB.
fib(1,Fib1) :- 
   const(fib1,Fib1),
   !.
fib(0,Fib0) :- 
   const(fib0,Fib0).

% -----------------------------------------------------------------------------
% Proceed bottom-up, without using any cache, or rather a cache consisting
% of two additional arguments (in effect, an accumulator that is a pair
% of arguments).
%
% ?- fib_bottomup_direct(10,F).
% F = 55.
% -----------------------------------------------------------------------------

fib_bottomup_direct(N,F) :-
   N>0,
   !,
   const(fib0,Fib0),
   const(fib1,Fib1),
   up(1,N,Fib0,Fib1,F).
fib_bottomup_direct(0,Fib0) :-
   const(fib0,Fib0).

% Tail recursive call moving "bottom up" towards N.
% In Java, I would have the reflex to avoid the last recursive
% call, but here the last "do-nothing" call looks natural.
%
% X:  the "current point of progress"
% N:  the N we want to reach
% FA: the value of fib(X-1)
% FB: the value of fib(X)
% F:  The variable that will receive the final result, fib(N)

up(X,N,FA,FB,F) :-
   X<N, % not there yet, compute fib(X+1)
   !,
   FC is FA + FB,
   Xn is X  + 1,
   up(Xn,N,FB,FC,F).
up(N,N,_,F,F).

% -----------------------------------------------------------------------------
% Exactly as fib_bottomup_direct/2, but you can specify your own fib(0) and 
% fib(1). "usv" stands for "unusual starter values".
% -----------------------------------------------------------------------------

fib_bottomup_direct_usv(N,F,Fib0,Fib1) :-
   N>0,
   !,
   up(1,N,Fib0,Fib1,F).
fib_bottomup_direct_usv(0,Fib0,Fib0,_Fib1).

% -----------------------------------------------------------------------------
% Proceed bottom-up, using an "associative array" (SWI-Prolog dict) as cache.
% The dict is used as an immutable but growing accumulator (functional style),
% and needs to be copied repeatedly (this is probably not optimizable as
% a dict is actually a compound term). At the end we have all the x:f(x)
% in the cache, so this is the way to go if you want to keep that map around
% for later.
% 
% ?- fib_bottomup_dict_cache(10,F,Cache).
% F = 55,
% Cache = cache{0:0,1:1,2:1,3:2,4:3,5:5,6:8,7:13,8:21,9:34,10:55}.
% -----------------------------------------------------------------------------

fib_bottomup_dict_cache(N,F,CacheFinal) :-
   const(fib0,Fib0),
   const(fib1,Fib1),
   up_dict(1,N,cache{0:Fib0,1:Fib1},CacheFinal),
   get_dict(N,CacheFinal,F).

% Tail recursive call moving "bottom up" towards N
%
% X         :  the "current point of progress"
% N         :  the N we want to reach
% Cache     : Contains all the mappings K:fib(K) for K =< X
% CacheFinal: The variable which will receive the final cache

up_dict(X,N,Cache,CacheFinal) :-
   X<N, % not there yet, compute fib(X+1)
   !,
   NA is X-1,
   NB is X,
   FC is Cache.NA + Cache.NB,    % retrieve from cache using . notation
   Xn is X + 1,
   put_dict(Xn,Cache,FC,Cache2), % build new dict with new Xn:fib(Xn) mapping
   up_dict(Xn,N,Cache2,CacheFinal).
up_dict(N,N,Cache,Cache).
up_dict(1,0,Cache,Cache).        % special case if 0 is queried

% -----------------------------------------------------------------------------
% Proceed bottom-up by defining a N+1-sized list of vars, with
% a frozen goal to compute fib(X) on the variable at place X for every 
% X > 1. Once that has been done, we bind the variables at places 0 and 1
% and the frozen goals unfreeze in turn for higher and higher X. 
%
% I'm not sure when this way of doing it would be useful; this is actually
% the same as what happens when using CLP(FD), but without using the 
% correct abstraction..
%
% ?- fib_bottomup_frozen_cache(10,Fib10,Cache).
% Fib10 = 55,
% Cache = [0,1,1,2,3,5,8,13,21,34,55].
%
% or with debugging on:
%
% ?- fib_bottomup_frozen_cache(10,Fib10,Cache).
% Starting computation
% Unfrozen: FB = 1
% Unfrozen: FB = 1
% Unfrozen: FB = 2
% Unfrozen: FB = 3
% Unfrozen: FB = 5
% Unfrozen: FB = 8
% Unfrozen: FB = 13
% Unfrozen: FB = 21
% Unfrozen: FB = 34
% Fib10 = 55,
% Cache = [0,1,1,2,3,5,8,13,21,34,55].
%
% -----------
%
% Note that the goal of freeze/2 is called when the term it is waiting on
% changes from var to nonvar, not when it is unified:
%
% Direct unfreeze:
%
% ?- freeze(2,format("Hello")).
% Hello
% true.
%
% Unfreeze when X becomes nonvar:
%
% ?- freeze(X,format("Hello")),X=2.
% Hello
% X = 2.
%
% No unfreeze when X is unified with another var:
%
% ?- freeze(X,format("Hello")),X=Y.
% X = Y,
% freeze(Y,format("Hello")).
%
% Unfreeze when X is unified with a nonground term and becomes nonvar:
%
% ?- freeze(X,format("Hello")),X=f(Y).
% Hello
% X = f(Y).
% -----------------------------------------------------------------------------

% :- debug(fib_freeze).

fib_bottomup_frozen_cache(N,F,Cache) :-
   1<N,
   !,
   M is N+1,
   length(Cache,M),
   setup_frozen_cache(Cache),
   const(fib0,Fib0),
   const(fib1,Fib1),
   debug(fib_freeze,"Starting computation",[]),
   % I am actually unsure whether there is some guaranteed that this unification
   % will be transactional so that the frozen goal always has all data when proceeding.
   % It *should* be.
   Cache=[Fib0,Fib1|_],
   nth0(N,Cache,F).
fib_bottomup_frozen_cache(1,Fib1,[Fib0,Fib1]) :-
   const(fib0,Fib0),
   const(fib1,Fib1).
fib_bottomup_frozen_cache(0,Fib0,[Fib0]) :- 
   const(fib0,Fib0).

setup_frozen_cache([FA,FB,FC|More]) :-   
   freeze(
      FB, % "trigger" variable: once FB is known, compute FC from FA and FB
      (
        debug(fib_freeze,"Unfrozen: FB = ~d",[FB]),
        (FC is FA+FB)
      )
   ), 
   setup_frozen_cache([FB,FC|More]).
setup_frozen_cache([_,_]). 

% -----------------------------------------------------------------------------
% Proceed "bottom-up" with a "lazy list" as cache. The "lazy list" is an 
% open list that has a frozen goal to compute the next list entry on its "fin".
%
% Retrieving a member of the list using nth0(N,Cache,F) causes unification of
% the unbound "fin" with a new listbox [_|_]. This thaws the goal on the 
% "fin", which then computes the next Fibonacci number, unifies it with arg1
% of the listbox and then sets up a new frozen goal on arg2 of the listbox, the
% new "fin" of the lazy list.
%
% It is not directly evident why this works with nth0/3, so a replacement
% predicate retrieve/3 which also prints debugging messages has been provided.
%
% This idea comes from http://rosettacode.org/wiki/Fibonacci_sequence#Prolog
%
% Example:
%
% ?- debug(fib_bll).
% ?- fib_bottomup_lazylist_cache(10,F,Cache).
% At this point, the cache just contains [0,1|_]: [0,1|_28196]
% At K = 0, N = 10, No unfreeze at this point
% At K = 1, N = 10, No unfreeze at this point
% At K = 2, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 0, FB = 1, FIN has been unified to [1|_28628]
% At K = 3, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 1, FB = 1, FIN has been unified to [2|_28910]
% At K = 4, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 1, FB = 2, FIN has been unified to [3|_29192]
% At K = 5, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 2, FB = 3, FIN has been unified to [5|_29474]
% At K = 6, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 3, FB = 5, FIN has been unified to [8|_29756]
% At K = 7, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 5, FB = 8, FIN has been unified to [13|_30038]
% At K = 8, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 8, FB = 13, FIN has been unified to [21|_30320]
% At K = 9, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 13, FB = 21, FIN has been unified to [34|_30602]
% At K = 10, N = 10, Will call unification with a listbox that will unfreeze the goal
% Unfrozen: FA = 21, FB = 34, FIN has been unified to [55|_30884]
% Unfrozen: FA = 21, FB = 34, FIN has been unified to [55|_30884]
% F = 55,
% Cache = [0,1,1,2,3,5,8,13,21,34,55|_31458],
% freeze(_31458,fibonacci_algorithms:bll_frozen(34,55,_31458)).
%
% Note that for call to retrieve/3, where K==N, the same debug message is issued
% twice. This is because "retrieve_3(K,N,[_|More],F)" is attempted first, leading to
% thawing, but then a rollback is issued due to K<N, and the frozen goal is
% reinstated. The second clause "retrieve_3(N,N,[F|_],F)" is then attempted, 
% leading to the same thawing. Side-effects in Prolog: interesting.
%
% This approach allows you to widen the cache on request, too. For example,
% if I want fib(10) first, but then also fib(13) I can just reuse the cache,
% lengthening it:
%
% ?- fib_bottomup_lazylist_cache(10,Fib10,Cache),nth0(13,Cache,Fib13).
% Fib10 = 55,
% Cache = [0,1,1,2,3,5,8,13,21,34,55,89,144,233|_55906],
% Fib13 = 233,
% freeze(_55906,fibonacci_algorithms:bll_frozen(144,233,_55906)).
%
% Note the residual goal being printed out.
% -----------------------------------------------------------------------------

% :- debug(fib_bll).

fib_bottomup_lazylist_cache(N,F,Cache) :-
   const(fib0,Fib0),
   const(fib1,Fib1),
   Cache=[Fib0,Fib1|Fin],
   freeze(
      Fin,
      bll_frozen(Fib0,Fib1,Fin)),
   debug(fib_bll,"At this point, the cache just contains: ~q",Cache),
   % nth0(N,Cache,F). % works too, but let's use retrieve/3 
   retrieve(N,Cache,F).

bll_frozen(FA,FB,FIN) :-
   FC is FA + FB,
   FIN=[FC|NewFIN],
   debug(fib_bll,"Unfrozen: FA = ~d, FB = ~d, FIN has been unified to ~q",[FA,FB,FIN]),
   freeze(
      NewFIN,
      bll_frozen(FB,FC,NewFIN)).

% A replacement for nth0/3 to show what's going on

retrieve(N,Cache,F) :-
   retrieve_2(0,N,Cache,F).

retrieve_2(K,N,Cache,F) :-
   (var(Cache)
    -> debug(fib_bll,"At K = ~d, N = ~d, Will call unification with a listbox that will unfreeze the goal",[K,N])
    ;  debug(fib_bll,"At K = ~d, N = ~d, No unfreeze at this point",[K,N])), 
   retrieve_3(K,N,Cache,F).

retrieve_3(K,N,[_|More],F) :-
   K < N,
   !,
   Kp is K+1,
   retrieve_2(Kp,N,More,F).
retrieve_3(N,N,[F|_],F).

% -----------------------------------------------------------------------------
% This algorithm based on a post by "Mostowski Collapse" at
% https://stackoverflow.com/questions/67972830/prolog-finding-the-nth-fibonacci-number-using-accumulators/
% which references
% https://kukuruku.co/post/the-nth-fibonacci-number-in-olog-n/ 
%
% The original code is licensed by StackOverflow as CC BY-SA 4.0
% according to https://stackoverflow.com/legal/terms-of-service#licensing
%
% Modified to follow the definition and not assume that fib(0) = 0, fib(1) = 1.
% 
% The principle is based on a matrix identity provided by Donald Knuth 
% (in Donald E. Knuth. The Art of Computer Programming. Volume 1. Fundamental 
%  Algorithms, page 80 of the second edition.)
%
% For n >= 1 and the standard Fibonacci sequence:
%
%                                 n
% [ fib(n+1) fib(n)   ]   [ 1  1 ]
% [                   ] = [      ]
% [ fib(n)   fib(n-1) ]   [ 1  0 ]
%
% But if we work with a non-standard Fibonacci sequence where we have no 
% assurance that fib(0)=0 and fib(1)=1 then for n >= 1 (a power of 0 on
% the right-hand matrix yields the identity matrix):
%
%                                                      n-1
% [ fib(n+1) fib(n)   ]   [ fib(2) fib(1) ]   [ 1  1 ]
% [                   ] = [               ] * [      ]
% [ fib(n)   fib(n-1) ]   [ fib(1) fib(0) ]   [ 1  0 ]
%
% Note that it is unimportant whether we multiply on the left of right
% because the two matrixes are symmetric matrixes of real numbers and the
% product is symmetric, so these matrixes must commute: to see why, take the 
% transpose on each side.
%
% This algorithm is actually O(log(N)): it doesn't care about computing
% all the intermediate values the O(N) algorithms compute. It goes directly
% from the constants fib(0), fib(1) to the result after computing the 
% matrix to apply. 
%
% Note the call to matrixpow/3:
%
% First argument is the power we want to apply, Pow >= 0
% Second argument is the standard Fibonacci sequence start matrix [ 1  1 ] 
% with fib(2) top left.                                           [ 1  0 ]
% Initially, the accumulator is the identity matrix.
%                    
% The PowMx result is a symmetric matrix, and is the identity matrix for 
% Pow = 0 and otherwise for Pow > 0 a matrix with standard Fibonacci numbers:
%
% [ fib(n)   fib(n-1) ]
% [ fib(n-1) fib(n-2) ], where n = Pow+1
%
% Taking a matrix Mx to the Pow-th power is done by recursively computing 
% (Mx^2)^(Pow//2) with 1 additional multiplication by Mx for odd N. 
% This gives a tail recursive scheme. One could also compute (Mx^(Pow//2))^2
% but that would not be tail recursive.
%
% If your starter matrix is sure to be `[[1,1],[1,0]]` you can collapse the
% two operations matrixpow/3 followed by matrixmult/3 in the main predicate
% into a single call to matrixpow/3.
% -----------------------------------------------------------------------------

fib_matrixmult(N,F) :-
   N>=1,
   !,
   Pow is N-1,
   const(fib0,Fib0),
   const(fib1,Fib1),
   Fib2 is Fib0+Fib1,
   matrixpow(
      Pow,
      [[1,1],[1,0]],
      PowMx),        
   matrixmult(
      [[Fib2,Fib1],[Fib1,Fib0]],
      PowMx,
      [[_,F],[F,_]]).
fib_matrixmult(0,Fib0) :-
   const(fib0,Fib0).

matrixpow(Pow, Mx, Result) :-
   matrixpow_2(Pow, Mx, [[1,0],[0,1]], Result).

matrixpow_2(Pow, Mx, Accum, Result) :- 
   Pow > 0,
   Pow mod 2 =:= 1, 
   !,
   matrixmult(Mx, Accum, NewAccum), 
   Powm is Pow-1,
   matrixpow_2(Powm, Mx, NewAccum, Result). 
matrixpow_2(Pow, Mx, Accum, Result) :- 
   Pow > 0,
   Pow mod 2 =:= 0,
   !,
   HalfPow is Pow div 2, 
   matrixmult(Mx, Mx, MxSq), 
   matrixpow_2(HalfPow, MxSq, Accum, Result).
matrixpow_2(0, _, Accum, Accum).

matrixmult([[A11,A12],[A21,A22]], 
           [[B11,B12],[B21,B22]],
           [[C11,C12],[C21,C22]]) :-
   C11 is A11*B11+A12*B21,
   C12 is A11*B12+A12*B22,
   C21 is A21*B11+A22*B21,
   C22 is A21*B12+A22*B22.

% -----------------------------------------------------------------------------
% Exactly as fib_matrixmult/2, but you can specify your own fib(0) and 
% fib(1). "usv" stands for "unusual starter values".
% -----------------------------------------------------------------------------

fib_matrixmult_usv(N,F,Fib0,Fib1) :-
   N>=1,
   !,
   Pow is N-1,
   Fib2 is Fib0+Fib1,
   matrixpow(
      Pow,
      [[1,1],[1,0]], 
      PowMx),
   matrixmult(
      [[Fib2,Fib1],[Fib1,Fib0]],
      PowMx,
      [[_,F],[F,_]]).
fib_matrixmult_usv(0,Fib0,Fib0,_Fib1).

% -----------------------------------------------------------------------------
% This is a reformulation of fib_bottomup_matrixmult, using vectors to 
% get rid of redundant computations.
%
% This algorithm based on a post by "Mostowski Collapse" at
% https://stackoverflow.com/questions/67972830/prolog-finding-the-nth-fibonacci-number-using-accumulators/
%
% The original code is licensed by StackOverflow as CC BY-SA 4.0
% according to https://stackoverflow.com/legal/terms-of-service#licensing
%
% The idea is to get rid of redundant operations in matrixmult/3, by using
% the fact that all our matrices are symmetric and actually hold Fibonacci
% numbers:
%
% [ fib(n+1)  fib(n)   ]
% [                    ]
% [ fib(n)    fib(n-1) ]
%
% So, if we multiply matrices A and B to yield C, we always have something
% of this form (even in the starter case where B is the identity matrix):
%
% [ A1+A2  A1 ]   [ B1+B2  B1 ]   [ C1+C2  C1 ]
% [           ] * [           ] = [           ]
% [ A1     A2 ]   [ B1     B2 ]   [ C1     C2 ]
%
% We can just retain the second columns of each matrix w/o loss of
% information. The operation between these vectors is not some
% standard operation like multiplication, let's mark it with ><:

% We can just retain the second columns of each matrix w/o loss of info:
%
% [ A1 ]    [ B1 ]   [ C1 ] 
% [    ] >< [    ] = [    ] 
% [ A2 ]    [ B2 ]   [ C2 ]
%
% where:
%
% C1 = B1*(A1+A2) + B2*A1 or A1*(B1+B2) + A2*B1
% C2 = A1*B1 + A2*B2
% -----------------------------------------------------------------------------

fib_matrixmult_streamlined(N,F) :-
   N>=1,
   !,
   Pow is N-1,
   const(fib0,Fib0),
   const(fib1,Fib1),
   matrixpow_streamlined(
      Pow,
      v(1,0),
      PowVec),
   matrixmult_streamlined(
      v(Fib1,Fib0),
      PowVec,
      v(F,_)).
fib_matrixmult_streamlined(0,Fib0) :-
   const(fib0,Fib0).

matrixpow_streamlined(Pow, Vec, Result) :-
   matrixpow_streamlined_2(Pow, Vec, v(0,1), Result).

matrixpow_streamlined_2(Pow, Vec, Accum, Result) :- 
   Pow > 0,
   Pow mod 2 =:= 1, 
   !,
   matrixmult_streamlined(Vec, Accum, NewAccum), 
   Powm is Pow-1,
   matrixpow_streamlined_2(Powm, Vec, NewAccum, Result). 
matrixpow_streamlined_2(Pow, Vec, Accum, Result) :- 
   Pow > 0,
   Pow mod 2 =:= 0,
   !,
   HalfPow is Pow div 2, 
   matrixmult_streamlined(Vec, Vec, VecVec), 
   matrixpow_streamlined_2(HalfPow, VecVec, Accum, Result).
matrixpow_streamlined_2(0, _, Accum, Accum).

matrixmult_streamlined(v(A1,A2),v(B1,B2),v(C1,C2)) :-
   C1 is A1*(B1+B2) + A2*B1, 
   C2 is A1*B1 + A2*B2.

% -----------------------------------------------------------------------------
% Exactly as fib_matrixmult_streamlined/2, but you can specify your own fib(0) 
% and fib(1). "usv" stands for "unusual starter values".
% -----------------------------------------------------------------------------

fib_matrixmult_streamlined_usv(N,F,Fib0,Fib1) :-
   N>=1,
   !,
   Pow is N-1,
   matrixpow_streamlined(
      Pow,
      v(1,0),
      PowVec),
   matrixmult_streamlined(
      v(Fib1,Fib0),
      PowVec,
      v(F,_)).
fib_matrixmult_streamlined_usv(0,Fib0,Fib0,_Fib1).

% -----------------------------------------------------------------------------
% This is a reformulation of fib_bottomup_powvec by "slago" at
% https://stackoverflow.com/questions/67972830/prolog-finding-the-nth-fibonacci-number-using-accumulators/
%
% This "fast doubling" (see https://www.nayuki.io/page/fast-fibonacci-algorithms)
%
% The original code is licensed by StackOverflow as CC BY-SA 4.0
% according to https://stackoverflow.com/legal/terms-of-service#licensing
%  -----------------------------------------------------------------------------

fib_fast_doubling(0,0) :- !.
fib_fast_doubling(N,F) :-
    fastfast(N, [_,F]).

fastfast(1, [0, 1]) :- !.
fastfast(N, R) :-
    M is N // 2,
    fastfast(M, [A, B]),
    F1 is A^2   + B^2,
    F2 is 2*A*B + B^2,
    (   N mod 2 =:= 0
    ->  R = [F1, F2]
    ;   F3 is F1 + F2,
        R = [F2, F3]   ).

% -----------------------------------------------------------------------------
% Proceed "top-down", using a preallocated list of length N+1 as cache.
%
% fib(X) can be found at place X in the list (0-based).
%
% We could set up a special case for fib(0) and fib(1) to keep the cache
% at length 1 and 2 respectively, but we don't bother. The cache will
% always have at least length 2.
% 
% ?- fib_topdown_list_cache_ascending(10,Fib10,Cache).
% Fib10 = 55,
% Cache = [0,1,1,2,3,5,8,13,21,34,55].
% -----------------------------------------------------------------------------

fib_topdown_list_cache_ascending(N,F,Cache) :-
   M is max(N+1,2),   % max/2 to cover the cases N=0,N=1
   length(Cache,M),   % allocate cache fully
   const(fib0,Fib0),
   const(fib1,Fib1),
   Cache = [Fib0,Fib1|_],
   fill_FTLCA(N,F,Cache).

fill_FTLCA(N,F,Cache) :-
   nth0(N,Cache,F), % this certainly has a cost for high N
   (var(F) 
    -> 
    (
       NA is N-1, fill_FTLCA(NA,FA,Cache),    
       % this call is always just retrieval and can be coded as such
       % NB is N-2, fill_FTLCA(NB,FB,Cache), 
       NB is N-2, nth0(NB,Cache,FB),
       F is FA+FB   
    )
    ;
    true
   ).

% -----------------------------------------------------------------------------
% Proceed "top-down", using an associative array (SWI Prolog dict) as cache.
% A simple termination condition is assured by filling the cache immediately
% with the ground cases (instead of waiting for the topdown recursion to
% hit that specific case and only then fill it in)
%
% ?- fib_topdown_dict_cache(10,F,Cache).
% F = 55,
% Cache = cache{0:0,1:1,2:1,3:2,4:3,5:5,6:8,7:13,8:21,9:34,10:55}.
% -----------------------------------------------------------------------------

fib_topdown_dict_cache(N,F,CacheFinal) :-
   const(fib0,Fib0),
   const(fib1,Fib1),
   down_dict(N,cache{0:Fib0,1:Fib1},CacheFinal),
   get_dict(N,CacheFinal,F).

down_dict(N,Cache,Cache) :-
   get_dict(N,Cache,_F),  % success, the value already exists in the cache
   !.
down_dict(N,Cache,CacheFinal) :-
   NA is N-1, down_dict(NA,Cache,Cache2), 
   % the next call is not needed and can be replaced by a simple lookup
   % NB is N-2, down_dict(NB,Cache2,Cache3), 
   NB is N-2, Cache3 = Cache2,
   FC is Cache3.NA + Cache3.NB,      % retrieve from cache using . notation
   put_dict(N,Cache3,FC,CacheFinal). % build new dict with new N:FC mapping

% -----------------------------------------------------------------------------
% Proceed "top-down", using a growing-at-the-front list of length N+1 as cache.
%
% fib(X) can be found at place N-X in the list (0-based)
%
% ?- fib_topdown_list_cache_descending(10,Fib10,Cache).
% Fib10 = 55,
% Cache = [55,34,21,13,8,5,3,2,1,1,0].
% -----------------------------------------------------------------------------

fib_topdown_list_cache_descending(N,F,Cache) :-
   fill_FTLCD(N,Cache),
   Cache=[F|_].

fill_FTLCD(0,[Fib0]) :-
   !,
   const(fib0,Fib0).
fill_FTLCD(1,[Fib1|More]) :-
   !,
   const(fib1,Fib1),
   fill_FTLCD(0,More).
fill_FTLCD(_,[F|_]) :-
   nonvar(F),
   !.
fill_FTLCD(N,[F,FA,FB|More]) :-
   var(F),
   NA is N-1, fill_FTLCD(NA,[FA,FB|More]),
   % the next call is unnecessary as NB has necessarily been computed after return
   % from the previous call
   % NB is N-2, fill_FTLCD(NB,[FB|More])
   F is FA+FB.

% -----------------------------------------------------------------------------
% This is exactly the same as fib_topdown_list_cache_descending/3 but
% it avoids recursive calls by inspecting whether the call would be useful.
% -----------------------------------------------------------------------------

fib_topdown_list_cache_descending_cautious(N,F,Cache) :-
   fill_FTLCD_cautious(N,Cache),
   Cache=[F|_].

fill_FTLCD_cautious(0,[Fib0]) :-
   !,
   const(fib0,Fib0).
fill_FTLCD_cautious(1,[Fib1|More]) :-
   !,
   const(fib1,Fib1),
   fill_FTLCD_cautious(0,More). % no use trying to avoid this call
fill_FTLCD_cautious(N,[F,FA,FB|More]) :-
   assertion(var(F)),
   (var(FA) -> 
      ( NA is N-1, fill_FTLCD_cautious(NA,[FA,FB|More]) )
      ; true),
   F is FA+FB.

% -----------------------------------------------------------------------------
% This is the same as "fib_topdown_list_cache_descending" but:
%
% 1) Prints output using debug/3
% 2) The x values are maintained in the the list, too.
% 
% This generates output that is nice to look at:
%
% ?- fib_topdown_list_cache_descending_debug(6,F,Cache).
% >> N=6, _8010
% >>  N=5, [fib(5,_12430),fib(_12440,_12442)|_12436]
% >>   N=4, [fib(4,_12442),fib(_12612,_12614)|_12608]
% >>    N=3, [fib(3,_12614),fib(_12790,_12792)|_12786]
% >>     N=2, [fib(2,_12792),fib(_12974,_12976)|_12970]
% >>      N=1, [fib(1,_12976),fib(_13164,_13166)|_13160]
% >>       N=0, [fib(_13164,_13166)|_13160]
% <<       N=0, [fib(0,0)]
% <<      N=1, [fib(1,1),fib(0,0)]
% <<     N=2, [fib(2,1),fib(1,1),fib(0,0)]
% <<    N=3, [fib(3,2),fib(2,1),fib(1,1),fib(0,0)]
% <<   N=4, [fib(4,3),fib(3,2),fib(2,1),fib(1,1),fib(0,0)]
% <<  N=5, [fib(5,5),fib(4,3),fib(3,2),fib(2,1),fib(1,1),fib(0,0)]
% << N=6, [fib(6,8),fib(5,5),fib(4,3),fib(3,2),fib(2,1),fib(1,1),fib(0,0)]
% -----------------------------------------------------------------------------

:- debug(ftlcd).

fib_topdown_list_cache_descending_debug(N,F,Cache) :-
   fill_FTLCD_wrapper(N,N,Cache),
   Cache=[fib(N,F)|_].

spaces(Length,Str) :-
   length(Cs,Length),
   maplist(=(' '),Cs),
   string_chars(Str,Cs).

fill_FTLCD_wrapper(N,OverallN,Cache) :-
   Length is OverallN-N,
   spaces(Length,Spaces),
   debug(ftlcd,">> ~sN=~d, ~q",[Spaces,N,Cache]),
   fill_FTLCD(N,OverallN,Cache),
   debug(ftlcd,"<< ~sN=~d, ~q",[Spaces,N,Cache]).

fill_FTLCD(0,_OverallN,[fib(0,Fib0)]) :-
   !,
   const(fib0,Fib0).
fill_FTLCD(1,OverallN,[fib(1,Fib1)|More]) :-
   !,
   const(fib1,Fib1),
   fill_FTLCD_wrapper(0,OverallN,More).
fill_FTLCD(N,_OverallN,[fib(N,F)|_]) :-
   nonvar(F),
   !.
fill_FTLCD(N,OverallN,[fib(N,F),fib(NA,FA),fib(NB,FB)|More]) :-
   var(F),
   NA is N-1, fill_FTLCD_wrapper(NA,OverallN,[fib(NA,FA),fib(NB,FB)|More]),
   % the next call is unnecessary as NB has necessarily been computed after return
   % from the previous call
   % NB is N-2, fill_FTLCD_wrapper(NB,OverallN,[fib(NB,FB)|More]),
   F is FA+FB.

% -----------------------------------------------------------------------------
% Proceed "top-down", using a preallocated list of length N+1 as cache.
% CLP(FD) constraints are set up among the vars in the list. At the end,
% we bind the vars for fib(0) and fib(1) and all the entries in the list
% are computed.
%
% fib(X) can be found at place N-X in the list (0-based).
%
% ?- fib_topdown_list_cache_clpfd(10,Fib10,Cache).
% Fib10 = 55,
% Cache = [55,34,21,13,8,5,3,2,1,1,0].
% -----------------------------------------------------------------------------

fib_topdown_list_cache_clpfd(N,F,Cache) :-
   M is N+1, 
   length(Cache,M),            % allocate cache fully
   setup_constraints(Cache),
   Cache=[F|_].                % pick up solution

setup_constraints([Fib0]) :-     % covers the case of N=0 
   const(fib0,Fib0).
setup_constraints([Fib1,Fib0]) :-  % set up start of list & ground constraint network
   const(fib0,Fib0),
   const(fib1,Fib1).             % this launches computation
setup_constraints([F,FA,FB|More]) :-
   F #= FA+FB,
   setup_constraints([FA,FB|More]).
 
% -----------------------------------------------------------------------------
% Another one by Mostowski Collapse. This should be the "linear algebra" 
% approach (to be confirmed)
% -----------------------------------------------------------------------------

fib_golden_ratio(N, S) :-
   powrad(N,(1,1),(1,0),(_,X)),
   powrad(N,(1,-1),(1,0),(_,Y)),
   S is (X-Y)//2^N.

powrad(0, _, R, R) :- !.
powrad(N, A, R, S) :- 
   N rem 2 =\= 0, !,
   mulrad(A, R, H), 
   M is N//2, 
   mulrad(A, A, B), 
   powrad(M, B, H, S).
powrad(N, A, R, S) :-
   M is N//2, 
   mulrad(A, A, B), 
   powrad(M, B, R, S).

mulrad((A,B),(C,D),(E,F)) :-
   E is A*C+B*D*5,
   F is A*D+B*C.


