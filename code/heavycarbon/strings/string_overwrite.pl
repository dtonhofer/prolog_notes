% ==============================================================================
% Overwrite a string/atom "LowerText" with another string/atom "UpperText"
%
% Character position 0 is the first character of "LowerText".
%
% The "UpperText" is placed at position "UpperPos", which may be negative
% or beyond the end of "LowerText". Any gap is filled with spaces.
%
% If "CutLeft" is "true", the any characters at positions less than 0 are cut,
%
% If "CutRight" is "true", the any characters at positions larger or equal than
% the length of "LowerText" are cut.
%
% If "Want" is 'atom' you will get an atom, if 'string' you will get a string.
%
% There are two version with the same functionality:
%
% overwrite_using_chars/7: Does brute-force character-by-character processing 
% and is slow but easy to check.
%
% overwrite_using_runs/7: Does run-of-character processing and is a bit hairy,
% but fast.
% =============================================================================
% Running the tests: There should be a file "string_overwrite.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/strings/string_overwrite.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% 2021-01-19: Review
% 2021-01-29: Changes in naming, review overwrite_intro_assertions/7
% =============================================================================

:- module(heavycarbon_strings_string_overwrite,
          [
          overwrite_using_chars/7  % overwrite_using_chars(LowerText,UpperText,UpperPos,CutLeft,CutRight,Result,Want)
         ,overwrite_using_runs/7   % overwrite_using_runs(LowerText,UpperText,UpperPos,CutLeft,CutRight,Result,Want)
          ]).

:- use_module(library('heavycarbon/strings/stringy.pl')).
:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).

% ===
% Using chars
% ===

% EXPORTED
overwrite_using_chars(LowerText,UpperText,UpperPos,CutLeft,CutRight,Result,Want) :-
   overwrite_intro_assertions(LowerText,UpperText,UpperPos,CutLeft,CutRight,Result,Want),
   stringy_chars(LowerText,LowerChars,_),
   stringy_chars(UpperText,UpperChars,_),
   stringy_length(LowerText,LowerLen),
   stringy_length(UpperText,UpperLen),
   PrelimStartPos is min(UpperPos,0),
   PrelimEndPos   is max(LowerLen,UpperPos+UpperLen),
   ((CutLeft  == true) -> StartPos = 0      ; StartPos = PrelimStartPos),
   ((CutRight == true) -> EndPos = LowerLen ; EndPos = PrelimEndPos),
   UpperEnd is UpperPos+UpperLen,
   collect(StartPos,EndPos,UpperPos,UpperEnd,UpperChars,LowerChars,LowerLen,Tip,FinalFin),
   FinalFin=[], % close difflist
   result_in_what(Want,Tip,Result).

result_in_what(atom,Chars,Result)   :- atom_chars(Result,Chars).
result_in_what(string,Chars,Result) :- string_chars(Result,Chars).

collect(Pos,EndPos,_,_,_,_,_,Fin,Fin) :-
   Pos == EndPos,!.

collect(Pos,EndPos,UpperPos,UpperEnd,UpperChars,LowerChars,LowerLen,Fin,FinalFin) :-
   Pos < EndPos,
   !,
   ((UpperPos=<Pos, Pos<UpperEnd)
    ->
    (Index is Pos-UpperPos,nth0(Index,UpperChars,Char)) % use "upper" character if possible
    ;
    (0=<Pos, Pos<LowerLen)
    ->
    nth0(Pos,LowerChars,Char) % otherwise use "lower" character if possible
    ;
    Char=' '), % otherwise use space filler (1 character)
   Fin=[Char|NewFin], % append via the difflist idiom
   PosPP is Pos+1,
   collect(PosPP,EndPos,UpperPos,UpperEnd,UpperChars,LowerChars,LowerLen,NewFin,FinalFin).

% ===
% Using runs
% ===

% EXPORTED
overwrite_using_runs(LowerText,UpperText,UpperPos,CutLeft,CutRight,Result,Want) :-
   overwrite_intro_assertions(LowerText,UpperText,UpperPos,CutLeft,CutRight,Result,Want),
   stringy_length(LowerText,LowerLen),
   stringy_length(UpperText,UpperLen),
   UpperEnd is UpperPos+UpperLen,
   upper_completely_or_partially_on_positions_below_position0(UpperText,UpperPos,UpperEnd,CutLeft,R1,Want),
   filler_between_end_of_upper_and_position0(UpperEnd,CutLeft,R1,R2,Want),
   lower_visible_between_position0_and_start_of_upper(UpperPos,LowerLen,LowerText,R2,R3,Want),
   upper_covering_lower(UpperText,UpperPos,UpperEnd,LowerLen,R3,R4,Want),
   lower_visible_between_end_of_upper_and_end_of_lower(LowerText,UpperEnd,LowerLen,R4,R5,Want),
   filler_between_end_of_lower_and_start_of_upper(UpperPos,LowerLen,CutRight,R5,R6,Want),
   upper_completely_or_partially_on_the_right(UpperText,UpperPos,UpperLen,UpperEnd,LowerLen,CutRight,R6,Result,Want).


upper_completely_or_partially_on_positions_below_position0(UpperText,UpperPos,UpperEnd,CutLeft,Rnew,Want) :-
   (CutLeft == true ; 0 =< UpperPos)
   ->
   stringy_ensure("",Rnew,Want) % do nothing except returning the empty string/atom
   ;
   (Len is min(0,UpperEnd)-UpperPos,
    sub_atom(UpperText,0,Len,_,Run),     % this gives an atom
    stringy_ensure(Run,Rnew,Want)).      % gives what we "Want"

filler_between_end_of_upper_and_position0(UpperEnd,CutLeft,Rprev,Rnew,Want) :-
   (CutLeft == true ; 0 =< UpperEnd)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is -UpperEnd,
    string_of_spaces(Len,Run),              % gives a string
    stringy_concat(Rprev,Run,Rnew,Want)).   % gives what we "Want"

lower_visible_between_position0_and_start_of_upper(UpperPos,LowerLen,Lower,Rprev,Rnew,Want) :-
   (UpperPos =< 0 ; LowerLen == 0)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is min(LowerLen,UpperPos),
    sub_atom(Lower,0,Len,_,Run),            % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)).   % gives what we "Want"

upper_covering_lower(UpperText,UpperPos,UpperEnd,LowerLen,Rprev,Rnew,Want) :-
   (UpperEnd =< 0 ; LowerLen =< UpperPos)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (StartPos      is max(0,UpperPos),
    StartPosInUpper is -min(0,UpperPos),
    EndPos   is min(LowerLen,UpperEnd),
    Len      is EndPos-StartPos,
    sub_atom(UpperText,StartPosInUpper,Len,_,Run), % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)).          % gives what we "Want"

lower_visible_between_end_of_upper_and_end_of_lower(LowerText,UpperEnd,LowerLen,Rprev,Rnew,Want) :-
   (LowerLen =< UpperEnd)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is min(LowerLen,LowerLen-UpperEnd),
    StartPos is max(0,UpperEnd),
    sub_atom(LowerText,StartPos,Len,_,Run),   % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)).     % gives what we "Want"

filler_between_end_of_lower_and_start_of_upper(UpperPos,LowerLen,CutRight,Rprev,Rnew,Want) :-
   (UpperPos =< LowerLen ; CutRight == true)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is UpperPos-LowerLen,
    string_of_spaces(Len,Run),             % gives a string
    stringy_concat(Rprev,Run,Rnew,Want)).  % gives what we "Want"

upper_completely_or_partially_on_the_right(UpperText,UpperPos,UpperLen,UpperEnd,LowerLen,CutRight,Rprev,Rnew,Want) :-
   (UpperEnd =< LowerLen ; CutRight == true)
   ->
   (Rnew=Rprev) % do nothing
   ;
   (StartPos is max(LowerLen,UpperPos),
    Len      is UpperEnd-StartPos,
    StartPosInUpper is UpperLen-Len,
    sub_atom(UpperText,StartPosInUpper,Len,_,Run), % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)).          % gives what we "Want"

% ===
% Intro checking
% ===

overwrite_intro_assertions(LowerText,UpperText,UpperPos,CutLeft,CutRight,_Result,Want) :-
   assertion(stringy(LowerText)),
   assertion(stringy(UpperText)),
   assertion(integer(UpperPos)),
   assertion((CutLeft==true;CutLeft==false)),
   assertion((CutRight==true;CutRight==false)),
   assertion((Want==atom;Want==string)).
   % assertion(var(Result)). may not be needed
