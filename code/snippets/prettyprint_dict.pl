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
          prettyprint_dict/1
          ]).

prettyprint_dict(Dict) :-
   bagof(                                           % bagof fails if the dict is empty
      Key-Value,
      get_dict(Key,Dict,Value),  
      Flat),                               
   keysort(Flat,FlatSorted),
   max_key_length(FlatSorted,MaxKeyLength),
   build_format_string(MaxKeyLength,FormatString),
   format_dict(FlatSorted,FormatString).            % direct to STDOUT

max_key_length(Flat,MaxKeyLength) :-
   foldl(
      ([Key-_,FromLeft,ToRight]>>(atom_length(Key,KeyLength),ToRight is max(FromLeft,KeyLength))),
      Flat,
      0,
      MaxKeyLength).

build_format_string(MaxKeyLength,FormatString) :-
   FieldWidth is MaxKeyLength+1,
   % set tab ~|, print datum ~a, add tab expanding-filler (will thus left justify) ~t,
   % set next tab stop FieldWidth positions after the previous tab stop ~FieldWidth+,
   % print datum ~q, add tab expanding-filler (will thus left justify) ~t,
   % set next tab stop 5 positions after the previous tab stop, add newline ~n
   atomics_to_string(['  ~|~a~t~',FieldWidth,'+: ~q~t~5+~n'],FormatString).
   % format("Format string is: ~q\n",[FormatString]).

format_dict([Key-Value|More],FormatString) :-
   format(FormatString,[Key,Value]),
   format_dict(More,FormatString).

format_dict([],_).

