% =============================================================================
% Code to list the operators currently configured. Just call list_ops/0.
% =============================================================================
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% =============================================================================

list_ops :-
   bagof(PrecVal,Type^Name^current_op(PrecVal,Type,Name),AllPrecVals),
   sort(AllPrecVals,AllPrecValsSorted),  % duplicates have been removed!
   AllPrecValsSorted=[MinPrecVal|_],
   last(AllPrecValsSorted,MaxPrecVal),
   % use dict_create to be able to use values in variables as keys
   dict_create(Annotations,attr,[MinPrecVal-"strongest precedence, most 'leaflike'",MaxPrecVal-"weakest precedence, most 'rootlike'"]),
   reverse(AllPrecValsSorted,AllPrecValsSorted2), % "rootlike" precedence first
   by_precedence(AllPrecValsSorted2,Annotations).
   
% ---
% Section of the given precedence
% ---

by_precedence([Precedence|More],Annotations) :-
   format("Precedence value ~q",[Precedence]),
   (get_dict(Precedence, Annotations, Text) ->  format(": ~s~n",[Text]) ; format("~n",[])),   
   handle_ops_with_precedence(Precedence),
   by_precedence(More,Annotations).
   
by_precedence([],_).

% ---
% All ops of the given precedence
% ---

handle_ops_with_precedence(Precedence) :-
   bagof(
      Name-Type,
      current_op(Precedence,Type,Name),
      Ops),
   keysort(Ops,OpsSorted), % sorted by name
   max_name_length(OpsSorted,MaxNameLength),
   format_ops(OpsSorted,MaxNameLength).
   
format_ops([Name-Type|More],MaxNameLength) :-
   FieldWidth is MaxNameLength+2,
   % set tab ~|, print datum ~a, add tab expanding-filler (will thus left justify) ~t,
   % set next tab stop FieldWidth positions after the previous tab stop ~FieldWidth+,
   % print datum ~a, add tab expanding-filler (will thus left justify) ~t,    
   % set next tab stop 5 positions after the previous tab stop, add newline ~n
   atomics_to_string(['  ~|~a~t~',FieldWidth,'+~a~t~5+~n'],FormatString),
   % format("Format string ~q\n",[FormatString]),
   format(FormatString,[Name,Type]),   
   format_ops(More,MaxNameLength).
   
format_ops([],_).

max_name_length(OpsSorted,MaxNameLength) :-
   foldl(
      ([Name-_,FromLeft,ToRight]>>(atom_length(Name,Len),ToRight is max(FromLeft,Len))),
      OpsSorted,
      0,
      MaxNameLength).
   
