% =============================================================================
% Implement a Markov Network traversal using the "effect handler" infrastructure.
% =============================================================================

:- use_module('effect_handler.pl').

:- debug(markov).

% ===
% The network
% ===

% can_move(?FromNode,?ToNode)

can_move(s0 , s1).
can_move(s0 , s0).
can_move(s0 , s2).

can_move(s1 , s2).
can_move(s1 , end).

can_move(s2 , s1).
can_move(s2 , s0).

% move_to_next_state(+CurNode,-NextNode)

move_to_next_node(CurNode,NextNode) :-
   must_be(nonvar,CurNode),
   must_be(var,NextNode),
   bagof(X,can_move(CurNode,X),Bag),  % fails if no move possible
   random_member(NextNode,Bag).       % uniform probabilities for all destinations

% ===
% The "client code" which traverses a markov network. The state is of the form
% [Node,History] where Node is an atom naming the current markov network node
% and History is the reversed list of markov network nodes visited up to now.
% ===

markov :-
   get_state(CurState),
   debug(markov,"Client: Got state ~q",[CurState]),
   maybe_loop(CurState).

maybe_loop([CurNode,History]) :-
   CurNode \== end,
   !,
   move_to_next_node(CurNode,NextNode),
   Updated = [NextNode,[NextNode|History]],
   debug(markov,"Client: Putting state ~q",[Updated]),
   put_state(Updated),
   markov.   % loop around, without any local arguments!

maybe_loop([end,_]). % stop the loop, we are done

% ===
% Call the following at the toplevel.
% ===
% Notice the "Inversion of Control" pattern: The "client code" markov/0
% implements the actual traversal and is managed/called by the generic effect
% handler predicate with_state/3.

run(markov) :-
   with_state(markov,[s0,[s0]],Out),
   debug(markov,"Final state is ~q",[Out]).

