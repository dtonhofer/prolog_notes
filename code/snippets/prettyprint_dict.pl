% ============================================================================
% Dict prettyprinting (if the values and keys fit on one line)
% ============================================================================
% Load module with:
%
% ?- use_module(library('snippets/prettyprint_dict.pl')).
%
% TODO: Instead of printing directly to stdout, build an list of string
% to be output "later" by the caller.
% ============================================================================

:- module(snippets_prettyprint_dict,
          [
          dict_lines/3 % dict_lines(+Dict,-Lines,Placeholder)
          ]).

dict_lines(Dict,Lines,Placeholder) :-
   assertion((var(Lines);is_list(Lines))),
   assertion(is_dict(Dict)),
   assertion(atomic(Placeholder)),
   dict_pairs(Dict,_,Pairs),                  % Does NOT fail if Dict is empty. Pairs are already sorted-by-key
   ((Pairs==[]) -> fail ; true),              % ...so we do that explicitly
   max_key_width(Pairs,MaxKeyWidth),
   build_format_string(MaxKeyWidth,FormatString,Placeholder),
   format_dict(Pairs,FormatString,Lines).     % Output into "Lines" instead of side-effecting; "Lines" do not have newlines!

max_key_width(Pairs,MaxKeyWidth) :-
   foldl(in_foldl,Pairs,0,MaxKeyWidth).

in_foldl(Key-_,FromLeft,ToRight) :-
   atom_length(Key,KeyWidth),
   ToRight is max(FromLeft,KeyWidth).

build_format_string(MaxKeyWidth,FormatString,Placeholder) :-
   FieldWidth is MaxKeyWidth+1,
   % 1) Set tab ~|, print atom ~a, add tab expanding-filler (will thus "left justify") ~t,
   % 2) Set next tab stop "FieldWidth" positions after the previous tab stop: "~FieldWidth+",
   % 3) Print datum "~" + "Placeholder" (which is s, q, a, ...) , add tab expanding-filler (will thus left justify) ~t,
   % 4) set next tab stop 5 positions after the previous tab stop
   atomics_to_string(['~|~a~t~',FieldWidth,'+: ~', Placeholder, '~t~5+'],FormatString).
   % format("Format string is: ~q\n",[FormatString]).

format_dict([Key-Value|MorePairs],FormatString,[Line|MoreLines]) :-
   format(string(Line),FormatString,[Key,Value]),
   format_dict(MorePairs,FormatString,MoreLines).

format_dict([],_,[]).

