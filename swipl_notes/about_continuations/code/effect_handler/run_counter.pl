% =============================================================================
% Implement a simple counter using the "effect handler" infrastructure.
% =============================================================================
% Run with ?- run_counter.

:- use_module('effect_handler.pl').

:- debug(count).

% ===
% The counting "client code". The state S is of the form [String,Integer] where
% the Integer is a counting variable that counts down to 0 and the String
% accumulates the values that the counting variable took on.
% ===

count :-
   get_state([Text,Counter]),
   debug(count,"Client: Got state ~q",[[Text,Counter]]),
   maybe_loop(Text,Counter).

maybe_loop(Text,Counter) :-
   Counter >= 0,
   !,
   NewCounter is Counter-1,
   string_extend(Text,Counter,NewText),
   debug(count,"Client: Putting state ~q",[[NewText,NewCounter]]),
   put_state([NewText,NewCounter]),
   count.  % loop around, without any local arguments!

maybe_loop(_,_).  % stop the loop, we are done!

% ---
% Helper
% ---

string_extend("",Counter,NewText) :-
   !,string_concat("",Counter,NewText).

string_extend(Text,Counter,NewText) :-
   string_concat(Text,"+",Text2),
   string_concat(Text2,Counter,NewText).

% ===
% Call the following at the toplevel.
% ===
% Notice the "Inversion of Control" pattern: The "client code" count/0
% implements the actual counting and is managed/called by the generic effect
% handler predicate with_state/3.

run_counter :-
   with_state(count,["",4],Out),
   debug(count,"Final state is ~q",[Out]).

