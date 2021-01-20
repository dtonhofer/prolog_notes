% =============================================================================
% Straightforward "string justification"
% =============================================================================
% Those little predicates may be called a lot of times (e.g. when formatting 
% tables), so let's not become too slow when running it!
% (Currently, there are meta-calls in there and checks and everything. Oh
%  well, why have GHz CPUs when you can't use them!)
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under: 
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Latest review: Tue 19 January 2021
% =============================================================================

:- module(heavycarbon_strings_justify,
   [
       justify_left/3    % justify_left(Text,Width,Result)
      ,justify_right/3   % justify_right(Text,Width,Result)
      ,justify_center/3  % justify_center(Text,Width,Result)
      ,justify_left/5    % justify_left(Text,Width,Result,Want,Nocheck)
      ,justify_right/5   % justify_right(Text,Width,Result,Want,Nocheck)
      ,justify_center/5  % justify_center(Text,Width,Result,Want,Nocheck)
      ,justify/10        % justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck)
   ]).

:- use_module(library('heavycarbon/support/meta_helpers.pl')).
:- use_module(library('heavycarbon/strings/stringy.pl')). 
:- use_module(library('heavycarbon/strings/string_of_spaces.pl')). 
:- use_module(library('heavycarbon/strings/string_overwrite.pl')). 

% ===
% Simple calls that don't check and always return strings
%
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% Result   : The Result, a string (not an atom)
% ===

justify_left(Text,Width,Result) :-
   justify(Text,Width,left,_,_,_,_,Result,string,false).

justify_right(Text,Width,Result) :-
   justify(Text,Width,right,_,_,_,_,Result,string,false).

justify_center(Text,Width,Result) :-
   justify(Text,Width,center,_,_,_,_,Result,string,false).

% ===
% Simple calls with reduced parameter count
%
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% Result   : The Result, an atom or a string depending on 'Want'
%
% Want     : One of 'atom' or 'string' to indicate what the "Result" should be. No default is accepted.
% Nocheck  : Bypass assertions on intro (if they haven't been compiled-out already). If 'true', then bypass. Anything else, including _: do not bypass.
% ===

justify_left(Text,Width,Result,Want,Nocheck) :-
   justify(Text,Width,left,_,_,_,_,Result,Want,Nocheck).

justify_right(Text,Width,Result,Want,Nocheck) :-
   justify(Text,Width,right,_,_,_,_,Result,Want,Nocheck).

justify_center(Text,Width,Result,Want,Nocheck) :-
   justify(Text,Width,center,_,_,_,_,Result,Want,Nocheck).

% ===
% Perform text justification, the whole enchilada.
%
% (Prolog abosolutely needs name-based parameter passing. Productions systems traditionally have them!)
%
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% How      : On of the atoms 'left', 'right', 'center' or leave it at _, in which case 'left' is assumed (and unified).
% Want     : One of 'atom' or 'string' to indicate what the "Result" should be. No default is accepted.
% CutLeft  : Cut off on the left (lower than position 0?). One of 'true' or 'false' or leave it at _, in which case 'true' is assumed (and unified).
% CutRight : Cut off on the right (higher than position Width?). One of 'true' or 'false' or leave it at _, in which case 'true' is assumed (and unified).
% Prefer   : In case of "How" = 'center', should the mass of the text be moved leftwards or rightwards if there is 1 leftover character? One of 'left' or 'right' or leave it at _, in which case 'left' is assumed (and unified).
% Offset   : An offset to apply in any case. For a positive value: When How == 'left', move rightwards, When How == 'right', move leftwards, When How == 'center', move leftwards. Leave at _ for 0.
% Nocheck  : Bypass assertions on intro (if they haven't been compiled-out already). If 'true', then bypass. Anything else, including _: do not bypass.
% Result   : The Result, an atom or a string depending on 'Want'
% ===

justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck) :-
   unless((Nocheck == true),assertions_intro_for_justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Want)),
   unless((var(Result);stringy(Result)),fail), % shortcut in case of "checking the result"
   if_then(var(How),How=left),
   if_then(var(CutLeft),CutLeft=true),
   if_then(var(CutRight),CutRight=true),
   if_then(var(Prefer),Prefer=left),
   if_then(var(Offset),Offset=0),
   justify_helper(How,Prefer,Text,Width,Offset,CutLeft,CutRight,Result,Want).

justify_helper(left,_,Text,Width,Offset,CutLeft,CutRight,Result,Want) :-
   string_of_spaces(Width,Spaces), % should use Want
   overwrite_using_runs(Spaces,Text,Offset,CutLeft,CutRight,Result,Want).

justify_helper(right,_,Text,Width,Offset,CutLeft,CutRight,Result,Want) :-
   string_of_spaces(Width,Spaces), % should use Want
   stringy_length(Text,TextLen),
   ActualOffset is Width-TextLen-Offset,
   overwrite_using_runs(Spaces,Text,ActualOffset,CutLeft,CutRight,Result,Want).

justify_helper(center,Prefer,Text,Width,Offset,CutLeft,CutRight,Result,Want) :-
   string_of_spaces(Width,Spaces), % should use Want
   stringy_length(Text,TextLen),
   reify(odd(TextLen),IsOddTextLen),
   reify(odd(Width),IsOddWidth),
   actual_offset(IsOddTextLen,IsOddWidth,TextLen,Width,Offset,Prefer,ActualOffset),
   overwrite_using_runs(Spaces,Text,ActualOffset,CutLeft,CutRight,Result,Want).

% ---
% Computing the actual offset to apply in case of a "centrally justified text"
%
%
%   oooooooooXiiiiiiiii       width is odd (and has a central character)
%         aaaXbbb             text is odd (and has a central character)
%                             ActualOffset := (Width-1)/2 - (TextLen-1)/2 + Offset
%
%   ooooooooooiiiiiiiiii      width is even
%          aaabbb             text is even
%                             ActualOffset := Width/2 - TextLen/2 + Offset
%
%   ooooooooooiiiiiiiiii      width is even
%          aaaXbbb            rightly behaviour : text is odd (and has a central character): Correction =  0 (default)
%         aaaXbbb             leftly  behaviour : text is odd (and has a central character): Correction = -1
%                             ActualOffset := Width/2 - (TextLen-1)/2 + Correction + (user-requested offset)
%
%   oooooooooXiiiiiiiii       width is odd (and has a central character)
%          aaabbb             rightly behaviour : text is even: Correction = +1
%         aaabbb              leftly  behviour  : text is even: Correction =  0 (default)
%                             ActualOffset := (Width-1)/2 - (TextLen-1)/2 + Correction + (user-requested offset)
% ---

% :- debug(actual_offset).

actual_offset(IsOddTextLen,IsOddWidth,TextLen,Width,Offset,Prefer,ActualOffset) :-
   correction(Prefer,IsOddTextLen,IsOddWidth,Correction),
   % debug(actual_offset,"TextLen = ~d ~q  Width = ~d ~q",[TextLen,IsOddTextLen,Width,IsOddWidth]),
   (IsOddWidth   -> (HalfWidth    is (Width-1)/2)   ; (HalfWidth    is Width/2)), assertion(integer(HalfWidth)),
   (IsOddTextLen -> (HalfTextLen  is (TextLen-1)/2) ; (HalfTextLen  is TextLen/2)), assertion(integer(HalfTextLen)),
   ActualOffset is (HalfWidth - HalfTextLen + Correction + Offset).

% ---
% Determining the correction (0,-1,+1) top apply to a "centrally justified text"
% depending on various factors
% ---

correction(left,true,false,-1) :- !.
correction(right,false,true,1) :- !.
correction(_,_,_,0).

% ---
% Assertions about arguments passed to justify
% Pass them all, disregard those in which we are not interested.
% ---

assertions_intro_for_justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Want) :-
   assertion(stringy(Text)),
   assertion((integer(Width), Width >= 0)),
   assertion((var(How);memberchk(How,[left,right,center]))),
   assertion((var(CutLeft);memberchk(CutLeft,[true,false]))),
   assertion((var(CutRight);memberchk(CutRight,[true,false]))),
   assertion((var(Prefer);memberchk(Prefer,[left,right]))),
   assertion((var(Offset);integer(Offset))),
   assertion(memberchk(Want,[atom,string])).

% ---
% Simple helpers
% ---

odd(X)  :- 1 =:= (X mod 2).

