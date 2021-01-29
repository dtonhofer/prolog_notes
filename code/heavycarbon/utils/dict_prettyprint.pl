% ============================================================================
% Dict prettyprinting (if the values and keys fit on one line)
% ============================================================================
% Load module with:
%
% ?- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).
%
% as long as the directory 'heavycarbon' is on the library path of 'swipl'·
%
% To run tests:
%
% ?- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).
% ?- load_test_files([]).
% ?- run_tests.
%
%
% dict_lines(+Dict,+Placeholder)
%
%   Prettyprint the "Dict" directly to "current_output" (i.e. print it
%   with format/2). The "Placeholder" is used in the format/2 formatting
%   string to format dict values. For example, if there are strings, 
%   use "s", if there are integers, use "d". If in doubt, use "q".
%   As usual, output can be captured/redirected to anotehr stream 
%   by using with_output_to/2 around the dict_lines/2 goal.
%
% dict_lines(+Dict,+Placeholder,?Lines)
%
%   Prettyprint the "Dict" to a list-of-strings, "Lines". The 
%   lines do not include a terminating newline. Useful if you want to
%   "generate now, but side-effect later".
%
% dict_lines_framed(+Dict,+Placeholder,+SettingsDict)
%
%   All the lines will have the same length, and have been
%   suitably padded with space on the right.
%   You can add additional padding and maybe a border around
%   the output by filling the "SettingsDict" appropriately
%   (if the dict does not have a key, hardcoded defaults are
%   chosen instead):
%
%   pad_left   : additional spaces on the left (default 0)
%   pad_right  : additional spaces on the right (default 0)
%   pad_top    : additional spaces on top (default 0)
%   pad_btm    : additional spaces on btm (default 0)
%   border     : 'true' or 'false': but an ASCII border around it
%              (default false)
%
% dict_lines_framed(+Dict,+Placeholder,+SettingsDict,?FramedLines)
%
%   As above, only printed to a list-of-string, "FramedLines"
%
% ----
% 
% Examples (see also the plunit test code)
%
% ?- dict_lines(_{a:1,b:2},d,Lines).
% Lines = ["a : 1  ","b : 2  "].
%
% ?- dict_lines(_{a:"hello world",b:"foo bar baz"},s,Lines).
% Lines = ["a : hello world","b : foo bar baz"].
%
% ?- dict_lines(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},e).
% w    : 2.598476e-01
% ww   : 1.458760e+00
% www  : 6.437649e+08
% wwww : 4.000000e+02
%
% ?- dict_lines_framed(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},e,
%    _{border:true}).
% +-------------------+
% |w    : 2.598476e-01|
% |ww   : 1.458760e+00|
% |www  : 6.437649e+08|
% |wwww : 4.000000e+02|
% +-------------------+
%
% ?- dict_lines_framed(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},e,
%    _{border:true,pad_left:1,pad_right:1}).
% +---------------------+
% | w    : 2.598476e-01 |
% | ww   : 1.458760e+00 |
% | www  : 6.437649e+08 |
% | wwww : 4.000000e+02 |
% +---------------------+
%
% ?- dict_lines_framed(_{w: 0.25984759, ww: 1.4587598, www: 643764856, wwww: 400},e,
%    _{border:true,pad_left:1,pad_right:1,pad_top:1,pad_btm:1}).
% +---------------------+
% |                     |
% | w    : 2.598476e-01 |
% | ww   : 1.458760e+00 |
% | www  : 6.437649e+08 |
% | wwww : 4.000000e+02 |
% |                     |
% +---------------------+
% ============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% 2021-01-29: First complete release
% =============================================================================

:- module(heavycarbon_utils_dict_prettyprint,
          [
           dict_lines/2        % dict_lines(+Dict,+Placeholder)
          ,dict_lines/3        % dict_lines(+Dict,+Placeholder,-?Lines)
          ,dict_lines_framed/3 % dict_lines_framed(+Dict,+Placeholder,+SettingsDict)
          ,dict_lines_framed/4 % dict_lines_framed(+Dict,+Placeholder,+SettingsDict,-FramedLines)
          ]).

:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).
:- use_module(library('heavycarbon/strings/string_overwrite.pl')).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(debug)).

% ===
% Output directly using format/2, i.e. write to "current_output". 
% Use with with_output_to/2 to redirect the output.
%
% Does not fail if "Dict" is empty. It just does nothing in that case.
% 
% dict_lines_direct(+Dict,+Placeholder)
% ===

% EXPORT
dict_lines(Dict,Placeholder) :-
   dict_lines(Dict,Placeholder,Lines),
   maplist([Line]>>format("~s~n",[Line]),Lines).
 
% ===
% All generated output is put into a list, which is unified with "Lines".
% After the call, "Lines" is a list of string. The lines do not have a
% newline at their end (yet). 
%
% Does not fail if "Dict" is empty. It just does nothing in that case.
%
% dict_lines(+Dict,+Placeholder,?Lines)
% ===

% EXPORT
dict_lines(Dict,Placeholder,Lines) :-
   assertion((var(Lines);is_list(Lines))),
   assertion(is_dict(Dict)),
   assertion(atomic(Placeholder)),
   dict_pairs(Dict,_,Pairs),                   % Does NOT fail if Dict is empty. Pairs is sorted.
   (Pairs==[]
    -> Lines=[]
    ;  dict_lines_2(Pairs,Placeholder,Lines)). % Continue only if there is content

dict_lines_2(Pairs,Placeholder,Lines) :-      
   max_key_width(Pairs,MaxKeyWidth),
   build_format_string(MaxKeyWidth,FormatString,Placeholder),
   format_pairs_iter(Pairs,FormatString,Lines). 

max_key_width(Pairs,MaxKeyWidth) :-
   foldl(in_foldl_for_pair,Pairs,0,MaxKeyWidth).

in_foldl_for_pair(Key-_,FromLeft,ToRight) :-
   atom_length(Key,KeyWidth), % Key is an atom or a "small" (actually very large) integer
   ToRight is max(FromLeft,KeyWidth).

build_format_string(MaxKeyWidth,FormatString,Placeholder) :-
   FieldWidth is MaxKeyWidth+1,
   % 1) Set tab ~|, print atom ~a, add tab expanding-filler (will thus "left justify") ~t,
   % 2) Set next tab stop "FieldWidth" positions after the previous tab stop: "~FieldWidth+",
   % 3) Print datum "~" + "Placeholder" (which is s, q, a, ...) , add tab expanding-filler (will thus left justify) ~t,
   % 4) set next tab stop 5 positions after the previous tab stop
   atomics_to_string(['~|~a~t~',FieldWidth,'+: ~', Placeholder, '~t~5+'],FormatString),
   debug(dict_prettyprint,"Format string is: '~s'~n",[FormatString]).

% ---
% The iteration that formats key-value pairs into a list of string (i.e. lines)
% ---

format_pairs_iter([Key-Value|MorePairs],FormatString,[Line|MoreLines]) :-
   format(string(Line),FormatString,[Key,Value]),
   format_pairs_iter(MorePairs,FormatString,MoreLines).

format_pairs_iter([],_,[]).

% ===
% Put a "frame" around the output. The output goes directly to "current_output"
% ===

% EXPORT
dict_lines_framed(Dict,Placeholder,SettingsDict) :-
   dict_lines_framed(Dict,Placeholder,SettingsDict,FramedLines),
   maplist([Line]>>format("~s~n",[Line]),FramedLines).
 
% ===
% Put a "frame" around the output. The output goes to "FramedLines"
% ===

% EXPORT
dict_lines_framed(Dict,Placeholder,SettingsDict,FramedLines) :-
   assertion(is_dict(SettingsDict)),
   get_setting(SettingsDict,pad_top,PadTop,0),
   get_setting(SettingsDict,pad_btm,PadBtm,0),
   get_setting(SettingsDict,pad_left,PadLeft,0),
   get_setting(SettingsDict,pad_right,PadRight,0),
   get_setting(SettingsDict,border,BorderFlag,false),
   assertion(maplist([X]>>(integer(X),X>=0),[PadTop,PadBtm,PadLeft,PadRight])),
   assertion((BorderFlag==false;BorderFlag==true)),
   dict_lines(Dict,Placeholder,Lines),                     % this gives the basic output; note that "Lines" may be []
   max_line_width(Lines,MaxLineWidth),                     % may give MaxLineWidth==0 
   PaddedWidth is MaxLineWidth + PadLeft + PadRight,       % how wide a line is when padding is considered
   dict_lines_framed_2(BorderFlag,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,FramedLines).

% ---
% Frame lines if "Border" is true
% ---

dict_lines_framed_2(true,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,FramedLines) :-
   top_or_bottom_border_line(PaddedWidth,LimitLine),             % create (one instance) of the "+-----+" line to put at the top and bottom
   background_string(true,PaddedWidth,BgString),                 % create (one instance) of the "|     |" background string
   FramedLines=[LimitLine|Fin0],                                 % start working on the "FramedLines" list, which becomes a difflist FrameLines-Fin0
   pad_around(PadTop,BgString,Fin0,Fin1),                        % continue working on the "FrameLines" list, but now on the difflist part Fin0-Fin1
   CorrPadLeft is PadLeft+1,                                     % add 1 because we have a border "|" on the left
   framed_iter(Lines,BgString,CorrPadLeft,Fin1,Fin2),            % continue working on the "FrameLines" list, but now on the difflist part Fin1-Fin2
   pad_around(PadBtm,BgString,Fin2,[LimitLine]).                 % after bottom-padding, close the open "FrameLines" list by unifying with [LimitLine]

% ---
% Frame lines if "Border" is false
% ---

dict_lines_framed_2(false,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,FramedLines) :-
   background_string(false,PaddedWidth,BgString),                % create (one instance) of the "       " background string
   pad_around(PadTop,BgString,FramedLines,Fin1),                 % start working on the "FramedLines" list, which becomes a difflist FrameLines-Fin1
   framed_iter(Lines,BgString,PadLeft,Fin1,Fin2),                % continue working on the "FrameLines" list, but now on the difflist part Fin1-Fin2
   pad_around(PadBtm,BgString,Fin2,[]).                          % after bottom-padding, close the open "FrameLines" list by unifying with []

% ---
% Iteration for top or bottom padding
% pad_around(PadCount,BgString,Fin,FinalFin)
% ---

pad_around(0,_BgString,FinalFin,FinalFin) :- !.

pad_around(PadCount,BgString,[BgString|NewFin],FinalFin) :- 
   assertion(PadCount>0),
   PadCountMinus is PadCount-1,
   pad_around(PadCountMinus,BgString,NewFin,FinalFin).
  
% ----
% The iteration over the basic output lines to construct the "framed lines".
% On each line, the background string "BgString" (which contains the padding whitespace 
% and maybe a border) is overwritten by "Line" at position "Pos".
% ---

framed_iter([Line|MoreLines],BgString,Pos,[FramedLine|MoreFramedLines],FinalFin) :-
   overwrite_using_runs(BgString,Line,Pos,true,true,FramedLine,string),
   framed_iter(MoreLines,BgString,Pos,MoreFramedLines,FinalFin).
 
framed_iter([],_BgString,_Pos,FinalFin,FinalFin). 

% ---
% Create a "background string" that looks like "|      |" with "PaddedWidth"
% whitespace in between "|" if "Border" is true and like "     " with
% "PaddedWidth" spaces otherwise.
%
% background_string(+Border:boolean,+PaddedWidth:int,-Line:String)
% ---

background_string(false,PaddedWidth,Line) :-
   string_of_spaces(PaddedWidth,Line).
   
background_string(true,PaddedWidth,Line) :-
   string_of_spaces(PaddedWidth,Str1),
   atomics_to_string(["|",Str1,"|"],Line).
   
% ---
% Create a "top/bottom-border string" that looks like "+--------+"
% There are "PaddedWidth" dashes surrounded by "+" (this is only called
% if "Border" is true)
% ---

top_or_bottom_border_line(PaddedWidth,Line) :-
   length(Chars,PaddedWidth),
   maplist(=("-"),Chars),                   % "Chars" is now "PaddedWidth" dashes
   atomics_to_string(Chars,Str1),           % Messy & Ugly
   atomics_to_string(["+",Str1,"+"],Line).
 
% ----
% Find max of line-width over all lines using foldl/4
% ---

max_line_width(Lines,MaxLineWidth) :- 
   foldl(in_foldl_for_line,Lines,0,MaxLineWidth).

in_foldl_for_line(Line,FromLeft,ToRight) :-
   string_length(Line,LineWidth),
   ToRight is max(FromLeft,LineWidth).

% ---
% Getting the "Value" for "Key" out of dict "Settings". If tit is missing, "Default" is assumed
% ---

get_setting(Settings,Key,Value,Default) :-
  get_dict(Key,Settings,Value) -> true ; Value = Default.



