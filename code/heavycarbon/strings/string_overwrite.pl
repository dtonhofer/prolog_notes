:- module(heavycarbon_strings_string_overwrite,
   [
      overwrite_using_chars/7  % overwrite_using_chars(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want)
      overwrite_using_runs/7   % overwrite_using_runs(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want)         
   ]).

:- use_module(library('heavycarbon/strings/stringy.pl')).

% ==============================================================================
% Overwrite a string/atom "Lower" with another string/atom "Upper"
% Character position 0 is the first character of "Lower".
% The "Upper" string is placed at position "UpperPos", which may be negative
% or beyond the end of "Lower". Any gap is filled with spaces.
% If "CutLeft" is "true", the any characters at positions less than 0 are cut,
% If "CutRight" is "true", the any characters at positions larger or equal than
% the length of "Lower" are cut.
% If "Want" is 'atom' you will get an atom.
% If "Want" is 'string' you will get a string.
%
% There are two version with the same functionality:
%
% One which does brute-force character-by-character processing and is slow but
% easy to check.
%
% One which does run-of-character processing and is a bit hairy, but fast.
% ==============================================================================
 
% ===
% Using chars
% ===

overwrite_using_chars(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want) :-
   overwrite_intro_assertions(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want),
   stringy_chars(Lower,LowerChars,_),
   stringy_chars(Upper,UpperChars,_),
   stringy_length(Lower,LowerLen),
   stringy_length(Upper,UpperLen),
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

overwrite_using_runs(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want) :-
   overwrite_intro_assertions(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want),
   stringy_length(Lower,LowerLen),
   stringy_length(Upper,UpperLen),
   UpperEnd is UpperPos+UpperLen,
   upper_completely_or_partially_on_positions_below_position0(Upper,UpperPos,UpperEnd,CutLeft,R1,Want),
   filler_between_end_of_upper_and_position0(UpperEnd,CutLeft,R1,R2,Want),
   lower_visible_between_position0_and_start_of_upper(UpperPos,LowerLen,Lower,R2,R3,Want),
   upper_covering_lower(Upper,UpperPos,UpperEnd,LowerLen,R3,R4,Want),
   lower_visible_between_end_of_upper_and_end_of_lower(Lower,UpperEnd,LowerLen,R4,R5,Want),
   filler_between_end_of_lower_and_start_of_upper(UpperPos,LowerLen,CutRight,R5,R6,Want),
   upper_completely_or_partially_on_the_right(Upper,UpperPos,UpperLen,UpperEnd,LowerLen,CutRight,R6,Result,Want).

   
upper_completely_or_partially_on_positions_below_position0(Upper,UpperPos,UpperEnd,CutLeft,Rnew,Want) :-
   (CutLeft == true ; 0 =< UpperPos) 
   ->
   stringy_ensure("",Rnew,Want) % do nothing except returning the empty string/atom
   ;
   (Len is min(0,UpperEnd)-UpperPos,
    sub_atom(Upper,0,Len,_,Run),     % this gives an atom
    stringy_ensure(Run,Rnew,Want)).  % gives what we "Want"

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

upper_covering_lower(Upper,UpperPos,UpperEnd,LowerLen,Rprev,Rnew,Want) :-
   (UpperEnd =< 0 ; LowerLen =< UpperPos)
   -> 
   (Rnew=Rprev) % do nothing
   ;
   (StartPos      is max(0,UpperPos),
    StartPosInUpper is -min(0,UpperPos),
    EndPos   is min(LowerLen,UpperEnd),
    Len      is EndPos-StartPos,
    sub_atom(Upper,StartPosInUpper,Len,_,Run), % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)).      % gives what we "Want"

lower_visible_between_end_of_upper_and_end_of_lower(Lower,UpperEnd,LowerLen,Rprev,Rnew,Want) :-
   (LowerLen =< UpperEnd)
   -> 
   (Rnew=Rprev) % do nothing
   ;
   (Len is min(LowerLen,LowerLen-UpperEnd),
    StartPos is max(0,UpperEnd),
    sub_atom(Lower,StartPos,Len,_,Run),   % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)). % gives what we "Want"
 
filler_between_end_of_lower_and_start_of_upper(UpperPos,LowerLen,CutRight,Rprev,Rnew,Want) :-
   (UpperPos =< LowerLen ; CutRight == true) 
   ->
   (Rnew=Rprev) % do nothing
   ;
   (Len is UpperPos-LowerLen,
    string_of_spaces(Len,Run),             % gives a string
    stringy_concat(Rprev,Run,Rnew,Want)).  % gives what we "Want"

upper_completely_or_partially_on_the_right(Upper,UpperPos,UpperLen,UpperEnd,LowerLen,CutRight,Rprev,Rnew,Want) :-
   (UpperEnd =< LowerLen ; CutRight == true) 
   ->
   (Rnew=Rprev) % do nothing
   ;
   (StartPos is max(LowerLen,UpperPos),
    Len      is UpperEnd-StartPos,
    StartPosInUpper is UpperLen-Len,
    sub_atom(Upper,StartPosInUpper,Len,_,Run), % gives an atom
    stringy_concat(Rprev,Run,Rnew,Want)).      % gives what we "Want"

% ===
% Into checking
% ===

overwrite_intro_assertions(Lower,Upper,UpperPos,CutLeft,CutRight,Result,Want) :-
   assertion(stringy(Lower)),
   assertion(stringy(Upper)),
   assertion(integer(UpperPos)),
   assertion((atom(CutLeft),memberchk(CutLeft,[true,false]))),
   assertion((atom(CutRight),memberchk(CutRight,[true,false]))),
   assertion((atom(Want),memberchk(Want,[atom,string]))),
   assertion(var(Result)).
