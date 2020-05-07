% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-17
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
% 
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
% 
% For more information, please refer to <http://unlicense.org/>
% ============================================================================
% A problem from:
%
% "Using Prolog to solve a brain teaser (Master Mind)"
% by haroldcampbell
% https://stackoverflow.com/questions/61276283/using-prolog-to-solve-a-brain-teaser-master-mind
% 
% This is a "Master Mind" problem with 10 colors (digits 0..9)
% and 3 positions.
%
% A simple generate-and-test method is used to solve it.
% The above post has another solution by Will Ness.
% ============================================================================

:- use_module(library(clpfd)).

% This anchors the values of A,B,C to the digits

base([A,B,C])  :- member(A,[0,1,2,3,4,5,6,7,8,9]),
                  member(B,[0,1,2,3,4,5,6,7,8,9]),
                  member(C,[0,1,2,3,4,5,6,7,8,9]).

% "291": one digit is right and in its place
%        (and of the other digits, none appears)
% "245": one digit is right but in the wrong place
%        (and of the other digits, none appears)
% "463": two digits are right but both are in the wrong place
%        (and the third digit does not appear)
% "578": all digits are wrong
%        (== none of them appears in the solution)
% "569": one digit is right but in the wrong place
%        (and of the other digits, none appears)

% Compare guess against clue and:
%
% - Count the number of digits that are "on the right place"
%   and discard them, keeping the part of the guess and clue as
%   "rest" for the next step.
% - Count the number of digits that are "on the wrong place"
%   and discard any pairings found, which is done with 
%   selectchk/3. If one uses member/2 as opposed to 
%   selectchk/2, the "wrong place counting" is, well, wrong.

% Note: - Decisions (guards and subsequent commits) made explicit
%         Usual style would be to share variables in the head instead,
%         then have a "green" or "red" cut as first occurence in the body.
%       - Incrementing the counter is done "early" by a constraint "#="
%         instead of on return by an effective increment,
%         because I feel like it (but is this worse efficiency-wise?)
%       - Explicit repetiton of "selectchk/3" before the green cut,
%         because I want the Cut to stay Green (Could the compiler 
%         optimized this away and insert a Red Cut in the preceding
%         clause? Probably not because Prolog does not carry enough
%         information for it to do so)

right_place_counting([],[],0,[],[]).

right_place_counting([G|Gs],[C|Cs],CountOut,Grest,Crest) :-
   G=C,
   !,
   CountOut#=CountMed+1,
   right_place_counting(Gs,Cs,CountMed,Grest,Crest).

right_place_counting([G|Gs],[C|Cs],CountOut,[G|Grest],[C|Crest]) :-
   G\=C,
   !,
   right_place_counting(Gs,Cs,CountOut,Grest,Crest).

% ---

wrong_place_counting([],_,0).

wrong_place_counting([G|Gs],Cs,CountOut) :-
    selectchk(G,Cs,CsRest),
    !,
    CountOut#=CountMed+1,
    wrong_place_counting(Gs,CsRest,CountMed).

wrong_place_counting([G|Gs],Cs,CountOut) :-
    \+selectchk(G,Cs,_),
    !,
    wrong_place_counting(Gs,Cs,CountOut).

% ---

counting(Guess,Clue,RightPlaceCount,WrongPlaceCount) :-
   right_place_counting(Guess,Clue,RightPlaceCount,Grest,Crest),
   wrong_place_counting(Grest,Crest,WrongPlaceCount).
                                                

clue1(Guess) :- counting(Guess,[2,9,1],1,0).
clue2(Guess) :- counting(Guess,[2,4,5],0,1).
clue3(Guess) :- counting(Guess,[4,6,3],0,2).
clue4(Guess) :- counting(Guess,[5,7,8],0,0).
clue5(Guess) :- counting(Guess,[5,6,9],0,1).

solution(L)  :- base(L),clue1(L),clue2(L),clue3(L),clue4(L),clue5(L).

