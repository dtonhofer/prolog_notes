% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-04-17
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
% Write a predicate which inserts a single integer element in an existing
% list of already-sorted integer elements, if append/ is available. Duplicates
% are discarded.
%
% This turns out to be relatively simple:, one scan is needed to find the 
% correct position, then one append/3 to destructure the input list,
% and one append/2 to generate the output list. 
%
% Of course, append will do its own scans ... how many?
%
% Vaguely based on:
% https://stackoverflow.com/questions/61277051/inserting-an-integer-into-a-sorted-list
% ============================================================================

insert_with_append(El,List,Out) :-
   search(El,List,0,Result),
   ((Result >= 0)
    ->
       (length(Front,Result),
        append(Front,Back,List),          % destructure input list
        append([Front,[El],Back],Out))    % generate output list
    ;
       Out = List % already exists in list
    ).

% search/4 looks for the insert position, counting the number of 
% items in List that come before Element, and setting CounterOut
% to that value, once known. CounterOut is set to -1 if Element
% is already in the list.

% search(Element,List,CounterCur,CounterOut)

search(_,[],Counter,Counter) :- !.

search(E,[L|Ls],CounterCur,CounterOut) :-
   L<E,!,
   succ(CounterCur,CounterNext),
   search(E,Ls,CounterNext,CounterOut).

search(E,[L|_],CounterCur,CounterCur) :-
   E<L,!.

search(E,[E|_],_,-1). 

% ---
% Unit tests
% ---

:-begin_tests(insert_with_append).

test(1) :- insert_with_append(10,[],R),R=[10].
test(2) :- insert_with_append(11,[2,3,5,7],R),R=[2,3,5,7,11].
test(3) :- insert_with_append(2,[3,5,7,11],R),R=[2,3,5,7,11].
test(4) :- insert_with_append(3,[2,3,5,7,11],R),R=[2,3,5,7,11].
test(5) :- insert_with_append(3,[2,5,7,11],R),R=[2,3,5,7,11].
test(6) :- insert_with_append(7,[2,3,5,11],R),R=[2,3,5,7,11].
test(7) :- insert_with_append(2,[],R),R=[2].

:-end_tests(insert_with_append).

rt :- run_tests(insert_with_append).
