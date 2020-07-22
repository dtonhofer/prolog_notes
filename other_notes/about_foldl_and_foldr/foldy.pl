% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-19
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
%
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
%
% For more information, please refer to <http://unlicense.org/>
% ============================================================================

:- use_module(library(clpfd)). % We are using #= instead of the raw "is".

% ===
% Implementations for some folding-f's of interest.
%
% - foldy_len:    Compute the length of the list by incrementing a counter.
% - foldy_add:    Adding all items in the list. Same results are obtained for "foldl" and "foldr"
%                 because "+" is nicely associative.
%                 Note that the unit test sets the starter value to the identity element of "+", i.e. 0.
% - foldy_mult:    Multiplying all items in the list. Same results are obtained for "foldl" and "foldr"
%                 because "*" is nicely associative.
%                 Note that the unit test sets the starter value to the identity element of "*", i.e. 1.
% - foldy_squadd: At each node, square the value obtained from the computation, then add the list item.
%                 Very different results for "foldl" and "foldr".
% - foldy_build:  Building a new structure (list) from the input list by "cons-ing" each item to
%                 the structure returned by the recursion.
%                 Note that "foldr" maps the input list identically to the output list, it is the
%                 identity operation, whereas "foldl" causes the list to be reversed (rather than
%                 "mirrored" for which one needs 1 more dimension).
% - foldy_join:   Building a string by joining items with ','. Very different results for "foldl"
%                 and "foldr".
% - foldy_expr:   Build a new structure that represents the expression that would be evaluated by
%                 the fold. In order to be flexible, the functor of the operation is taken by
%                 "foldy_expr" as first argument, and is passed by creating a term that is a
%                 "partially filled call" to be completed in the fold predicate. (weird;
%                 functional programming languages do this in a more consistent way).
% ===

foldy_len(_Item,In,Out) :-
   succ(In,Out).

foldy_add(Item,In,Out) :-
   Out #= Item+In.

foldy_mult(Item,In,Out) :-
   Out #= Item*In.

foldy_squadd(Item,In,Out) :-
   Out #= Item+(In^2).

% '[|]' is SWI-Prolog specific, replace by '.' as consbox constructor in other Prologs

foldy_build(Item,In,Out) :-
   Out = '[|]'(Item,In).

foldy_join(Item,In,Out) :-
   (In \= "")
   -> with_output_to(string(Out),format("~w,~w",[Item,In]))
   ;  with_output_to(string(Out),format("~w",[Item])).

% '=..' ("univ") constructs a term from a list of functor and arguments

foldy_expr(Functor,Item,In,Out) :-
   Out =.. [Functor,Item,In].
