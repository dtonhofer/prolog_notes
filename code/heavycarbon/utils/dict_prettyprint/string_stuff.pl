:- module(heavycarbon_utils_dict_prettyprint_string_stuff,
          [
           max_line_width/2                    % max_line_width(+Lines,-MaxLineWidth)
          ,make_tag_line/6                     % make_tag_line(+Tag,+MaxLineWidth,+PadLeft,+PadRight,+SettingsDict,-TagLineOut)
          ,make_bordery_lines/5                % make_bordery_lines(+MaxLineWidth,+PadLeft,+PadRight,-BorderLineOut,-BackgroundLineOut)
          ,make_background_line_for_padding/4  % make_background_line_for_padding(+MaxLineWIdth,+PadLeft,+PadRight,-LineOut)
          ]).

% ----------

/** <module> dict prettyprinter helper predicates

This module collects a few very simple and very specialized helper predicates
which just add noise to the main module. So they are here.

If you need to, load it manually with:

```
?- use_module(library('heavycarbon/utils/dict_prettyprint/string_stuff.pl')).
```

@license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
@author David Tonhofer (ronerycoder@gluino.name)

*/

:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).
:- use_module(library('heavycarbon/strings/stringy.pl')).
:- use_module(library('heavycarbon/strings/justify.pl')).
:- use_module(library('heavycarbon/utils/dict_prettyprint/settings.pl')).
:- use_module(library('heavycarbon/utils/dict_prettyprint/checking.pl')).

% ----------

%! max_line_width(+Lines,-MaxLineWidth)
%
% Find "maximum line width" over all lines (this means find "max string length" as the lines are strings)

max_line_width(Lines,MaxLineWidth) :-
   foldl(foldl_mlw,Lines,0,MaxLineWidth).

foldl_mlw(Line,FromLeft,ToRight) :-
   string_length(Line,LineWidth),
   ToRight is max(FromLeft,LineWidth).

% ----------

%! make_tag_line(+Tag,+MaxLineWidth,+PadLeft,+PadRight,+SettingsDict,-TagLineOut)
%
% Create the TagLineOut (a string) with the dict's tag Tag by justifying the tag
% inside that line according to the values passed. In principle, the length 
% of string Tag =< MaxLineWidth. At call time, Tag _should_ be an atom or an
% integer, but we cannot be sure, so we pump it through format/3 once. Note that
% is likely that both PadLeft and PadRight are 0.
%
% **Explainer for the various values:**
%
% ```
%   |<-----PadLeft--->|<---- MaxLineWidth ---->|<----PadRight----------->|
%   |                 |left_justified_tag      |                         |
%   |                 |     right_justified_tag|                         |
%   |                 |  center_justified_tag  |                         |
%   |full_left_justified_tag                   |                         |
%   |                 |                        | full_right_justified_tag|
%   |                 |    full_center_justified_tag                     |
% ```
% 
% @tbd justify_how/4 could be extended to not perform assertion checks internally. Maybe.
% @tbd justify_how/4 generates trailing spaces in "Result", which might be unwanted.

make_tag_line(Tag,MaxLineWidth,PadLeft,PadRight,SettingsDict,TagLineOut) :-
   get_setting(SettingsDict,justify_tag,How,center),
   get_setting(SettingsDict,justify_tag_full,Full,true),
   assertion(is_left_right_center(How)),
   assertion(is_boolean(Full)),
   preprocess_tag(Tag,TagString),
   make_tag_line_2(Full,How,TagString,MaxLineWidth,PadLeft,PadRight,TagLineOut).

preprocess_tag(Tag,TagString) :-
   format(string(TagString),"~q",[Tag]). % ~q always? good enough!

% ----------

% make_tag_line_2(Full,How,TagString,MaxLineWidth,PadLeft,PadRight,TagLineOut)
%
% Build the actual line containing the tag.
% Not exported.

make_tag_line_2(true,How,TagString,MaxLineWidth,PadLeft,PadRight,TagLineOut) :- 
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   justify_how(How,TagString,FullWidth,TagLineOut). % TODO in all cases whitespace over FullWidth, maybe unwanted?

make_tag_line_2(false,How,TagString,MaxLineWidth,PadLeft,_PadRight,TagLineOut) :- 
   justify_how(How,TagString,MaxLineWidth,M),    % TODO in all cases whitespace over MaxLineWidth, maybe unwanted?
   string_of_spaces(PadLeft,LS),                 % PadLeft may be 0.
   stringy_concat(LS,M,TagLineOut,string).       % Don't bother to append a string of "PadRight" spaces. 

% ----------

%! make_bordery_lines(+MaxLineWidth,+PadLeft,+PadRight,-BorderLineOut,-BackgroundLineOut)
% 
% A packaged call for both make_horizontal_border_line/2 and make_background_line_with_border/2
% in one step.

make_bordery_lines(MaxLineWidth,PadLeft,PadRight,BorderLineOut,BackgroundLineOut) :-
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   make_horizontal_border_line(FullWidth,BorderLineOut),           % create "+-----+" line to put at the top and bottom
   make_background_line_with_border(FullWidth,BackgroundLineOut).  % create "|     |" line to use as background

% ----------

% make_horizontal_border_line(+Width,-LineOut)
%
% Create a "horizontal border line" that looks like "+--------+".
% There are Width dashes surrounded by "+" for the corners. This predicate is only
% called if a border had been requested.
% Not exported.

make_horizontal_border_line(Width,LineOut) :-
   length(Chars,Width),
   maplist(=("-"),Chars),                        % "Chars" is now "Width" dashes
   atomics_to_string(Chars,S),                   % Fuse into a string
   stringy_concat("+",S,"+",LineOut,string).

% ----------

% make_background_line_with_border(+Width,-LineOut)
%
% Create a "background line" that looks like "|      |" with Width whitespace in between
% the surrounding "|". This predicate is only called if a border has been requested.
% Not exported.

make_background_line_with_border(Width,LineOut) :-
   string_of_spaces(Width,S),                
   stringy_concat("|",S,"|",LineOut,string).

% ----------

%! make_background_line_for_padding(+MaxLineWidth,+PadLeft,+PadRight,-LineOut)
%
% Create a "background line" for padding only: it's just whitespace.

make_background_line_for_padding(MaxLineWidth,PadLeft,PadRight,LineOut) :-
   FullWidth is PadLeft + MaxLineWidth + PadRight,
   string_of_spaces(FullWidth,LineOut). 

