:- module(splinter0,
          [
              splinter0/5   % splinter0(List, N, Prefix, Element, Suffix)

          ]).

% ============================================================================
% "Splinter a List by index, 0-based"
%
%   splinter0(+List, +N, ?Prefix, ?Element, ?Suffix)
%   splinter0(?List, ?N, +Prefix, +Element, +Suffix)
%
% Given a List and an index N into the list (N from 0 to N-1),
% splinter a list at position N into:
%
% - a prefix-list
% - the element at position N (which is why the index must be 0..N-1)
% - a suffix-list
%
% for example:
%
% [a,b,c,d,e,f,g,h] N=0 ==> [] a [b,c,d,e,f,g,h]
% [a,b,c,d,e,f,g,h] N=1 ==> [a] b [c,d,e,f,g,h]
% [a,b,c,d,e,f,g,h] N=2 ==> [a,b] c [d,e,f,g,h]
% [a,b,c,d,e,f,g,h] N=3 ==> [a,b,c] d [e,f,g,h]
% [a,b,c,d,e,f,g,h] N=4 ==> [a,b,c,d] e [f,g,h]
% [a,b,c,d,e,f,g,h] N=5 ==> [a,b,c,d,e] f [g,h]
% [a,b,c,d,e,f,g,h] N=6 ==> [a,b,c,d,e,f] g [h]
% [a,b,c,d,e,f,g,h] N=7 ==> [a,b,c,d,e,f,g] h []
%
% This is done by judicious application of append/2.
%
% Error responses:
%
% - a negative index causes a throw
% - an out-of-bounds positive or 0 index causes failure
% ============================================================================
% Consider the following possibilites:
% 1 = nonvar at that argument place
% 0 = var at at that argument place
% . = don't care
%
% Deterministic, anything additionally set allows consistency checks (conchecks)
%
% [1 1 . . .] - List, N        --> can compute/concheck Prefix, Element, Suffix ("work forwards")
% [1 . 1 . .] - List, Prefix   --> can compute/concheck N via length/2, then work forward
% [1 . . . 1] - List, Suffix   --> can compute N, and thus the rest
% [. . 1 . 1] - Prefix, Suffix --> can compute N, generate List as fresh variables ("work backwards")
%
% Patch up with fresh variables:
%
% [. 1 . . 1] - N,Suffix       --> can generate fresh Prefix and List
%
% Nondeterministic:
%
% [1 . . . .] - List           --> generate all N possibilities to splinter the list
%
% List of arbitrary length implied (all the cases that have not been caught above)
% In this case, throwing an error seems appropriate.
%
% [0 . . . 0] - Not enough info to limit length of List
% [0 . 0 . 1] - Not enough info to limit length of list
% ============================================================================

splinter0(List, N, Prefix, Element, Suffix) :-
    ground_flags([List, N, Prefix, Element, Suffix], GroundFlags),
    splinter0_dispatch(GroundFlags, List, N, Prefix, Element, Suffix).

% ---
% Handle cases depending on what variables are ground/nonground
% ---

splinter0_dispatch([1,1|_], List, N, Prefix, Element, Suffix) :-
   !,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([1,_,1|_], List, N, Prefix, Element, Suffix) :-
   !,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([1,_,_,_,1], List, N, Prefix, Element, Suffix) :-
   !,
   length(Suffix,B),
   length(List,L),
   N is L-B-1,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([_,_,1,_,1], List, N, Prefix, Element, Suffix) :-
   !,
   length(Suffix,B),
   length(Prefix,F),
   L is B+F+1,
   length(List,L), % generate fresh variables if var(List)
   N is F,
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([_,1,_,_,1], List, N, Prefix, Element, Suffix) :-
   !,
   length(Suffix,B),
   F is N,
   L is F+B+1,
   length(List,L), % generate fresh variables if var(List)
   splinter0_forward(List,N,Prefix,Element,Suffix).

splinter0_dispatch([1|_], List, N, Prefix, Element, Suffix) :-
   !,
   length(List,L),     % known list, known length
   succ(N_max,L),
   between(0,N_max,N), % generate Ns because "length(Prefix,N)" alone won't stop
   length(Prefix,N),   % know length, known list of fresh variables
   append([Prefix,[Element],Suffix],List).

splinter0_dispatch(GFs,_,_,_,_,_) :-
   with_output_to(string(Buffer),format("Not enough info to limit the length of the list! ground_flags = ~q",[GFs])),
   throw(error(Buffer)).

% ---
% The Workhorse. There are only two lines!
% ---

splinter0_forward(List, N, Prefix, Element, Suffix) :-
   length(Prefix,N),
   append(Prefix,[Element|Suffix],List).

% ---
% Helper: compute "ground flags"
% ---

ground_flags([A|As],[1|Flags]) :- nonvar(A),!,ground_flags(As,Flags).
ground_flags([A|As],[0|Flags]) :- var(A),!,ground_flags(As,Flags).
ground_flags([],[]).


