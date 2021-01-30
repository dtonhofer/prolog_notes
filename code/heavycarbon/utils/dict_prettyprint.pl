% ============================================================================
% Dict prettyprinting (if the values and keys fit on one line)
% ============================================================================
% Load module with:
% -----------------
%
% ?- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).
%
% as long as the directory 'heavycarbon' is on the library path of 'swipl'·
%
% To run tests:
% -------------
% 
% ?- use_module(library('heavycarbon/utils/dict_prettyprint.pl')).
% ?- load_test_files([]).
% ?- run_tests.
%
%
% Exports:
% --------
%
% Print "Dict" to current output, possibly with "SettingsDict":
%
%    dict_pp/1              % dict_pp(+Dict)
%    dict_pp/2              % dict_pp(+Dict,+SettingsDict)
%
% Do not print directly, instead generate a list of string:
%
%    dict_pp_lines/2        % dict_pp_lines(+Dict,-Lines)
%    dict_pp_lines/3        % dict_pp_lines(+Dict,+SettingsDict,-Lines)
%
% Print directly, and pad and/or box the outermost dict (subdicts
% may be boxed too, if "SettingsDict" contains "boxed:true")
%
%    dict_pp_padded/2       % dict_pp_padded(+Dict,+SettingsDict)
%
% As above, but do not print directly:
%
%    dict_pp_lines_padded/3 % dict_pp_lines_padded(+Dict,+SettingsDict,-paddedLines)
%
% Examples (see also the plunit test code)
% ----------------------------------------
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
%    _{border:true,pad_left:1,pad_right:1,pad_top:1,pad_btm:1}).
% +---------------------+
% |                     |
% | w    : 2.598476e-01 |
% | ww   : 1.458760e+00 |
% | www  : 6.437649e+08 |
% | wwww : 4.000000e+02 |
% |                     |
% +---------------------+
%
% RECURSIVE DICTS
%
% ?- dict_pp_padded(_{w1: 10, w2: 200, w3: 3000, 
%                     w4: _{w1: 10, w2: 20, 
%                        w3: _{ a: 12, b: 13}}},_{border:true}).
%
% +--------------------+
% |w1 : 10             |
% |w2 : 200            |
% |w3 : 3000           |
% |w4 : +-------------+|
% |     |w1 : 10      ||
% |     |w2 : 20      ||
% |     |w3 : +------+||
% |     |     |a : 12|||
% |     |     |b : 13|||
% |     |     +------+||
% |     +-------------+|
% +--------------------+
%
% Simpler:
%
% ?- dict_pp(_{w1: 10, w2: 200, w3: 3000, w4: _{w1: 10, w2: 20, w3: _{ a: 12, b: 13}}}).
% w1 : 10
% w2 : 200
% w3 : 3000
% w4 : w1 : 10    
%      w2 : 20    
%      w3 : a : 12
%           b : 13
%
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
           dict_pp/1              % dict_pp(+Dict)
          ,dict_pp/2              % dict_pp(+Dict,+SettingsDict)
          ,dict_pp_lines/2        % dict_pp_lines(+Dict,-Lines)
          ,dict_pp_lines/3        % dict_pp_lines(+Dict,+SettingsDict,-Lines)
          ,dict_pp_padded/2       % dict_pp_padded(+Dict,+SettingsDict)
          ,dict_pp_lines_padded/3 % dict_pp_lines_padded(+Dict,+SettingsDict,-paddedLines)
          ]).

:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).
:- use_module(library('heavycarbon/strings/string_overwrite.pl')).
:- use_module(library('heavycarbon/strings/stringy.pl')).
:- use_module(library('heavycarbon/strings/justify.pl')).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(debug)).

% ===
% Prettyprint "Dict" directly using format/2, i.e. write to "current_output". 
% Use with with_output_to/2 to redirect the output to a stream of choice.
%
% Behaves like dict_lines/3 followed by immediatey emission of output,
% and all setting set to default.
% 
% dict_pp(+Dict)
% ===

% EXPORT
dict_pp(Dict) :-
   dict_pp(Dict,_{}).

% ===
% Prettyprint "Dict" directly using format/2, i.e. write to "current_output". 
% Use with with_output_to/2 to redirect the output to a stream of choice.
% 
% Behaves like dict_lines/3 followed by immediatey emission of output.
% 
% dict_lines(+Dict,+SettingsDict)
% ===
  
% EXPORT
dict_pp(Dict,SettingsDict) :-
   dict_pp_lines(Dict,SettingsDict,Lines),
   maplist([Line]>>format("~s~n",[Line]),Lines). % All the lines are strings, so "~s"
 
% ===
% Prettyprint "Dict" by generating strings that are put into list and unified
% with "Lines", maybe for later emission to a stream. The lines do not have a 
% newline at their end.
%
% Also takes a dict with settings to configure prettyprinting, although
% for now, there are no settings. Pass an empty dict "_{}" here.
%
% Does not fail if "Dict" is empty. It just does nothing in that case.
%
% dict_lines(+Dict,+SettingsDict,?Lines)
% ===

% EXPORT
dict_pp_lines(Dict,Lines) :-
 dict_pp_lines(Dict,_{},Lines).
 
% EXPORT
dict_pp_lines(Dict,SettingsDict,Lines) :-
   assertion((var(Lines);is_list(Lines))),
   assertion(is_dict(Dict)),
   assertion(is_dict(SettingsDict)),
   dict_pairs(Dict,_,Pairs),   % Does NOT fail if Dict is empty. Pairs is sorted.
   ((Pairs==[]
    -> Lines=[]
     ;  pp_lines_2(Pairs,SettingsDict,Lines))), % Continue only if there is content
   assertion(is_list_of_string(Lines)).   

% ===
% Prettyprint "Dict" directly using format/2, i.e. write to "current_output". 
% Use with with_output_to/2 to redirect the output to a stream of choice.
%
% Behaves like dict_lines_padded/3 followed by immediatey emission of output.
%
% dict_lines_padded(+Dict,+SettingsDict)
% ===

% EXPORT
dict_pp_padded(Dict,SettingsDict) :-
   dict_pp_lines_padded(Dict,SettingsDict,PaddedLines),
   maplist([Line]>>format("~s~n",[Line]),PaddedLines). % they are all strings, so placeholder "~s"
 
% ===
% Prettyprint "Dict" by generating strings that are put into list and unified
% with "Lines", maybe for later emission to a stream. The lines do not have a
% newline at their end. 
%
% Pads the output with whitespace so that every line has the same 
% width with additional withespace added around that "rectangle of characters"
% according to the values in "SettingsDict". You can additionally request that
% a border made of dashes should enclose the "rectangle of characters". 
%
% The following settings exist. Missing settings assume default values.
%
%  pad_left   : int>=0,     default 0
%  pad_right  : int>=0,     default 0
%  pad_top    : int>=0,     default 0
%  pad_bottom : int>=0,     default 0
%  border     : boolean,    default false
%
% Does not fail if "Dict" is empty. It just does nothing in that case.
%
% dict_pp_lines_padded(+Dict,+SettingsDict,?PaddedLines)
% ===

% EXPORT
dict_pp_lines_padded(Dict,SettingsDict,PaddedLines) :-
   assertion(is_dict(SettingsDict)),
   get_setting(SettingsDict,pad_top,PadTop,0),
   get_setting(SettingsDict,pad_btm,PadBtm,0),
   get_setting(SettingsDict,pad_left,PadLeft,0),
   get_setting(SettingsDict,pad_right,PadRight,0),
   get_setting(SettingsDict,border,BorderFlag,false),
   assertion(is_list_of_positive_integers([PadTop,PadBtm,PadLeft,PadRight])),
   assertion(is_boolean(BorderFlag)),
   dict_pp_lines(Dict,SettingsDict,Lines),                 % this gives the basic output; "Lines" may be empty
   max_line_width(Lines,MaxLineWidth),                     % may give MaxLineWidth==0 
   PaddedWidth is MaxLineWidth + PadLeft + PadRight,       % how wide a line is when padding is considered
   padded_lines(BorderFlag,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,PaddedLines).

% ===
% Called from dict_pp_lines/3 to actually do something.
% ===

pp_lines_2(Pairs,SettingsDict,Lines) :-  
   assertion(Pairs \== []),
   pairs_to_entries(Pairs,SettingsDict,Entries,[]),            % from "Pairs" build "Entries" as an open list to be terminated with []
   max_key_width(Entries,MaxKeyWidth),                         % it is possible that MaxKeyWidth is 0 because '' is a valid key.
   max_monovalue_width(Entries,MaxMonoValueWidth),             % may also be 0
   lineify_entries(Entries,MaxKeyWidth,MaxMonoValueWidth,SettingsDict,Lines,[]). % iterate over entries, building "Lines" as an open list to be terminated with []

% ---
% Find the max key width in "Entries" which is a list "KeyString-Lineified"
% ---

max_key_width(Entries,MaxKeyWidth) :-
   foldl(in_foldl_max_key_width,Entries,0,MaxKeyWidth).

in_foldl_max_key_width(String-_,FromLeft,ToRight) :-
   string_length(String,Width),
   ToRight is max(FromLeft,Width).

% ---
% Find the max mono-value width in "Entries" which is a list "KeyString-Lineified"
% ---

max_monovalue_width(Entries,MaxMonoValueWidth) :-
   foldl(in_foldl_max_monovalue_width,Entries,0,MaxMonoValueWidth).

in_foldl_max_monovalue_width(_-mono(String),FromLeft,ToRight) :-  
   !, 
   string_length(String,Width),
   ToRight is max(FromLeft,Width).

in_foldl_max_monovalue_width(_-poly(_),PassThrough,PassThrough).

% ===
% Transforming entries from "Entries" which is a list "KeyString-Lineified"
% into "Lines" (i.e. strings). This is the final operation in pp_lines_2/3
%
% lineify_entries(Entries,MaxKeyWidth,MaxMonoValueWidth,SettingsDict,Lines,FinalFin).
% ===

lineify_entries([],_,_,_,FinalFin,FinalFin).

% case where "Lineified" contains only a string ("mono")

lineify_entries([KeyString-mono(MonoString)|MoreEntries],MaxKeyWidth,MaxMonoValueWidth,SettingsDict,[Line|MoreLines],FinalFin) :-
   !,
   justify_left(KeyString,MaxKeyWidth,KeyStringJustified),
   justify_mono_string(SettingsDict,MaxMonoValueWidth,MonoString,MonoStringJustified),
   stringy_concat(KeyStringJustified," : ",MonoStringJustified,Line,string),
   lineify_entries(MoreEntries,MaxKeyWidth,MaxMonoValueWidth,SettingsDict,MoreLines,FinalFin).
 
% case where "Lineified" contains multiple lines ("poly") but the count is actually 0

lineify_entries([KeyString-poly([])|MoreEntries],MaxKeyWidth,MaxMonoValueWidth,SettingsDict,[FirstLine|MoreLines],FinalFin) :-
   !,
   first_line(KeyString,MaxKeyWidth,"",FirstLine),
   lineify_entries(MoreEntries,MaxKeyWidth,MaxMonoValueWidth,SettingsDict,MoreLines,FinalFin).

% case where "Lineified" contains multiple lines ("poly") and the count is at least 1

lineify_entries([KeyString-poly([FirstSubLine|MoreSubLines])|MoreEntries],MaxKeyWidth,MaxMonoValueWidth,SettingsDict,Fin,FinalFin) :-
   first_line(KeyString,MaxKeyWidth,FirstSubLine,FirstLine),
   Fin=[FirstLine|Fin1],                                 % append this "FirstLine"
   filler_string(MaxKeyWidth,Filler),
   maplist_which_appends_to_open_list(                   % append the rest of the lines
      ({Filler}/[SubLine,Concatted]>>stringy_concat(Filler,SubLine,Concatted,string)),
      MoreSubLines,
      Fin1,
      Fin2),
   lineify_entries(MoreEntries,MaxKeyWidth,MaxMonoValueWidth,SettingsDict,Fin2,FinalFin).

% ---
% justify a "mono" string
% ---

% TODO: After justification, StringOut may have trailing spaces due to
% both a mix of trailing spaces in StringIn (ok) and the spaces of the
% field width "Width" because the justification is based on overwriting
% a field spaces of that width. One may not always want the latter!

justify_mono_string(SettingsDict,Width,StringIn,StringOut) :-
   get_setting(SettingsDict,justify,How,none),
   justify_how(How,StringIn,Width,StringOut).

% ---
% Construct the first resulting line for a "poly" case
% ---

first_line(KeyString,MaxKeyWidth,SubLine,FirstLine) :-
   justify_left(KeyString,MaxKeyWidth,KeyJustified),
   stringy_concat(KeyJustified," : ",SubLine,FirstLine,string).

% ---
% Construct a filler string for a "poly" case
% ---

filler_string(MaxKeyWidth,Filler) :-
   string_of_spaces(MaxKeyWidth,Spaces),      % from "string_of_spaces.pl"
   stringy_concat(Spaces,"   ",Filler,string).

% ===
% Iterating over the pairs found in a dict and transforming them into lines (strings)
%
% pairs_to_entries(Pairs,SettingsDict,Entries,FinalFin)
% === 
% This constructs "Entries" as a list (actually an open list with the fin unified to
% "FinalFin") structured as follows:
% - "Entries contains" the transformed pairs (i.e. the dict entries) in the order in
%   which they appear (and will be printed).
% - Each element in "Entries" is a pair "KeyString-Lineified" where:
% -- "KeyString" is a string generated from the dict key (which is always atom or small
%    integer). The caller will want to justify this string left or right.
% -- "Lineified" is a compound term:
%    mono(X) - "X" is a single string generated by formatting the value; it can be
%              printed "as is" (after suitable left/right justification)
%    poly(X) - "X" is a list of string generated by formatting a "child dict";
%              the strings need to be integrated into the lines generated for this
%              "parent dict"

pairs_to_entries(Pairs,SettingsDict,Entries,FinalFin) :-
   maplist_which_appends_to_open_list(
     ({SettingsDict}/[Key-Value,KeyString-Lineified]>>
        (stringify_key(Key,SettingsDict,KeyString),          % straightforward
         lineify_value(Value,SettingsDict,Lineified))),      % may cause recursion if the value is a dict
     Pairs,Entries,FinalFin).

% ---
% String generation for a dict key. "SettingsDict" is passed because there
% may be some parameters in there in a future version. No justification
% occurs here yet!
% ---

stringify_key(Key,_SettingsDict,String) :-
   (atom(Key) 
    -> F="~a"
    ;  integer(Key)
    -> F="~d"
    ;  domain_error([atom,integer],Key)), % never happens unless the implementation of dict changes
   format(string(String),F,[Key]).

% ---
% Multi-String generation (i.e. "lines generation") for a dict value.
% This may perform a recursive call if the "Value" is a dict in itself.
% "SettingsDict" is passed because there may be some parameters in there in a
% future version.
% ---

% TODO: limit recursive descent;

lineify_value(Value,SettingsDict,poly(Lines)) :-
   is_dict(Value),
   !,
   dict_pp_lines_padded(Value,SettingsDict,Lines). % recursive call! this structure better not be cyclic!
 
lineify_value(Value,SettingsDict,Lineified) :-
   % TODO: Rewrite the below to Prolog style..
   atom(Value)
   -> (format(string(String),"~a",[Value]), Lineified=mono(String))
   ;
   string(Value)
   -> (format(string(String),"~s",[Value]), Lineified=mono(String))
   ;
   integer(Value)
   -> (format(string(String),"~d",[Value]), Lineified=mono(String))
   ;
   var(Value)
   -> (format(string(String),"~q",[Value]), Lineified=mono(String))
   ;
   float(Value)
   -> (float_format(SettingsDict,Value,String), Lineified=mono(String))
   ;
   (format(string(String),"~q",[Value]),
       Lineified=mono(String)).

% ---
% Formatting a float. The placeholder is "f" unless there is a value
% registerewd in "SettingsDict" under 'float'. Note that if that is
% an illegal placeholder, format/2 will throw an exception. (TODO: Should we
% catch that exception locally?)
%
% Note that this is ONLY used for floats. Integers are NOT pass through here!
% ---

float_format(SettingsDict,Float,String) :-
   get_setting(SettingsDict,float,Placeholder,f),
   stringy_concat("~",Placeholder,FormatString,string),
   % format("FormatString is ~q\n",[FormatString]),
   format(string(String),FormatString,[Float]).
 
% ===
% Called from dict_pp_lines_padded/3 to pad the lines
% padded_lines(BorderFlag,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,PaddedLines)
% ===

% ---
% Pad lines in case "Border" is true
% ---

padded_lines(true,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,PaddedLines) :-
   top_or_bottom_border_line(PaddedWidth,LimitLine),             % create (one instance) of the "+-----+" line to put at the top and bottom
   background_string(true,PaddedWidth,BgString),                 % create (one instance) of the "|     |" background string
   PaddedLines=[LimitLine|Fin0],                                 % start working on the "PaddedLines" list, which becomes a difflist PaddedLines-Fin0
   pad_around(PadTop,BgString,Fin0,Fin1),                        % continue working on the "PaddedLines" list, but now on the difflist part Fin0-Fin1
   CorrPadLeft is PadLeft+1,                                     % add 1 because we have a border "|" on the left
   padded_iter(Lines,BgString,CorrPadLeft,Fin1,Fin2),            % continue working on the "PaddedLines" list, but now on the difflist part Fin1-Fin2
   pad_around(PadBtm,BgString,Fin2,[LimitLine]).                 % after bottom-padding, close the open "PaddedLines" list by unifying its fin with [LimitLine]

% ---
% Pad lines in case "Border" is false
% ---

padded_lines(false,Lines,PaddedWidth,PadLeft,PadTop,PadBtm,PaddedLines) :-
   background_string(false,PaddedWidth,BgString),                % create (one instance) of the "       " background string
   pad_around(PadTop,BgString,PaddedLines,Fin1),                 % start working on the "PaddedLines" list, which becomes a difflist FrameLines-Fin1
   padded_iter(Lines,BgString,PadLeft,Fin1,Fin2),                % continue working on the "PaddedLines" list, but now on the difflist part Fin1-Fin2
   pad_around(PadBtm,BgString,Fin2,[]).                          % after bottom-padding, close the open "PaddedLines" list by unifying its fin with []

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
% The iteration over the basic output lines to construct the "padded lines".
% On each line, the background string "BgString" (which contains the padding whitespace 
% and maybe a border) is overwritten by "Line" at position "Pos".
% ---

padded_iter([],_,_,FinalFin,FinalFin). 

padded_iter([Line|MoreLines],BgString,Pos,[PaddedLine|MorePaddedLines],FinalFin) :-
   overwrite_using_runs(BgString,Line,Pos,true,true,PaddedLine,string), % from module "string_overwrite.pl"
   padded_iter(MoreLines,BgString,Pos,MorePaddedLines,FinalFin).

% ---
% Create a "background string" that looks like "|      |" with "PaddedWidth"
% whitespace in between "|" if "Border" is true and like "     " with
% "PaddedWidth" spaces otherwise.
%
% background_string(+Border:boolean,+PaddedWidth:int,-Line:String)
% ---

background_string(false,PaddedWidth,Line) :-
   string_of_spaces(PaddedWidth,Line).  % from module "string_of_spaces.pl"
   
background_string(true,PaddedWidth,Line) :-
   string_of_spaces(PaddedWidth,Str1),  % from module "string_of_spaces.pl"
   stringy_concat("|",Str1,"|",Line,string).
   
% ---
% Create a "top/bottom-border string" that looks like "+--------+"
% There are "PaddedWidth" dashes surrounded by "+" (this is only called
% if "Border" is true)
% ---

top_or_bottom_border_line(PaddedWidth,Line) :-
   string_of_dashes(PaddedWidth,String),
   stringy_concat("+",String,"+",Line,string).
 
% ===
% Find max of linewidth over all lines using foldl/4
% ===

max_line_width(Lines,MaxLineWidth) :- 
   foldl(in_foldl_for_line,Lines,0,MaxLineWidth).

in_foldl_for_line(Line,FromLeft,ToRight) :-
   string_length(Line,LineWidth),
   ToRight is max(FromLeft,LineWidth).

% ===
% Getting the "Value" for "Key" out of dict "Settings". If tit is missing, "Default" is assumed
% ===

get_setting(SettingsDict,Key,Value,Default) :-
  get_dict(Key,SettingsDict,Value) -> true ; Value = Default.

% ===
% A useful helper that seems to be missing in Prolog
% ===

maplist_which_appends_to_open_list(_,[],FinalFin,FinalFin) :- !.
 
maplist_which_appends_to_open_list(Goal,[NominalIn|MoreNominalIns],[NominalOut|Fin],FinalFin) :-
   call(Goal,NominalIn,NominalOut),
   maplist_which_appends_to_open_list(Goal,MoreNominalIns,Fin,FinalFin).
 
% ===
% Assorted
% ===

string_of_dashes(Width,String) :-
   length(Chars,Width),
   maplist(=("-"),Chars),           % "Chars" is now "Width" dashes
   atomics_to_string(Chars,String).

is_list_of_positive_integers(L) :-
   maplist([X]>>(integer(X),X>=0),L).

is_list_of_string(L) :-
   maplist([X]>>(string(X)),L).

is_boolean(X) :-
   X==false;X==true.
 

