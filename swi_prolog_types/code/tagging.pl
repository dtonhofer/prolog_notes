% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-05-XX
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
% This program implements the "Prolog data type decision tree" at
% https://github.com/dtonhofer/prolog_notes/tree/master/swi_prolog_types/swi_prolog_type_tree
%
% - Efficiency is of no importance, we just want to implement the tree.
% - Unknown data types should lead to failure. Use assertion/1 to check conditions
%   as needed.
% - Cuts shall not be used. This means (empty) choicepoints stay open on
%   success. The unit tests are written to check that only one answer is
%   indeed returned.
% ============================================================================
% TODOs
%
% Test cyclic structures. How do I stop the descent?
%
% test(cyclic_1)     :- R=[1,2|S],S=[3,4|S],tag(S,X).
%
% Find out whether the compound is a graph or just a tree. Also test for open list
%
% test(graph)        :- R=f(A,B),A=g(X,Y),B=h(Y,X),tag(R,S).
% ============================================================================

:- use_module(library(clpfd)).  

% ===
% Unit tests; run with ?- rt(_).
% ===

:- begin_tests(tagging).

test(tag_var)      :- mono(tag(T)     , var(T)).
test(tag_int)      :- mono(tag(100)   , int(100)).
test(tag_float)    :- mono(tag(100.1) , float(100.1)).
test(tag_prat)     :- mono(tag(1r3)   , prat(1r3)).
test(slash)        :- mono(tag(1/3)   , compound(/, [gnd], [int(1), int(3)])).
test(atom_0)       :- mono(tag('')    , atomzero).
test(atom_1)       :- mono(tag(x)     , atom(x)).
test(string_0)     :- mono(tag("")    , stringzero).
test(string_1)     :- mono(tag("x")   , string("x")).
test(emptylist)    :- mono(tag([])    , emptylist).
test(compound_0)   :- mono(tag(a())   , compzero(a)).
test(dictfunctor)  :- dict_functor(F),
                      mono(tag(F)     ,dictfunctor).
test(dict)         :- mono(tag(d{x:1,y:1,z:Z}),
                      dict(d, [nongnd], [atom(x)-int(1), atom(y)-int(1), atom(z)-var(Z)])).
test(properlist_1) :- mono(tag([1]), lbox([list, gnd], int(1), emptylist)).
test(properlist_2) :- mono(tag([1,2]), lbox([list, gnd], int(1), lbox([list, gnd], int(2), emptylist))).
test(properlist_3) :- mono(tag([1,X,2]), lbox([list, nongnd], int(1), lbox([list, nongnd], var(X), lbox([list, gnd], int(2), emptylist)))).
test(openlist)     :- mono(tag([1,2,3|X]), lbox([nonlist, nongnd], 
                                                int(1), 
                                                lbox([nonlist, nongnd], 
                                                     int(2), 
                                                     lbox([nonlist, nongnd], 
                                                          int(3), var(X))))).
test(badlist)      :- mono(tag([1,2|huh]), lbox([nonlist, gnd], int(1), lbox([nonlist, gnd], int(2), atom(huh)))).
test(conj_2)       :- mono(tag((A,B)), conj([nongnd], var(A), var(B))).
test(conj_3)       :- mono(tag((1,2,3)), conj([gnd], int(1), conj([gnd], int(2), int(3)))).
test(pair_1)       :- mono(tag(A-B), pair([nongnd], var(A), var(B))).
test(pair_2)       :- mono(tag(1-2), pair([gnd], int(1), int(2))).
test(compound_1)   :- mono(tag(p(a,b,c)), compound(p, [gnd], [atom(a), atom(b), atom(c)])).
test(compound_2)   :- mono(tag(p(X,f(b,c))), compound(p, [nongnd], [var(X), compound(f, [gnd], [atom(b), atom(c)])])).
test(compound_3)   :- mono(tag(p(X,[1,2])), compound(p, [nongnd], [var(X), lbox([list, gnd], int(1), lbox([list, gnd], int(2), emptylist))])).

% ---
% Collect tagging results; there must be only one!
% ---

mono(Goal,Result) :- bagof(S, call(Goal,S), [Result]).

:- end_tests(tagging).

rt(tagging) :- run_tests(tagging).

% ============================================================================
% The business end
% ============================================================================

% ===
% Obtain the dict functor. (Is there a way to write it down directly?)
% ===

dict_functor(F) :- compound_name_arity(_{},F,_Arity).

% ===
% Tagging a term. Try to do this without cuts. There is no pretense at 
% efficiency.
% ===

% var vs nonvar

tag(T,var(T)) :- var(T).
tag(T,TT)     :- nonvar(T),tag_nonvar(T,TT).

% nonvar: atomic vs compound

tag_nonvar(T,TT) :- atomic(T),tag_atomic(T,TT).
tag_nonvar(T,TT) :- compound(T),tag_compound(T,TT).

% atomic: blob vs string vs number

tag_atomic(T,TT) :- string(T),tag_string(T,TT).
tag_atomic(T,TT) :- blob(T,_),tag_blob(T,TT).
tag_atomic(T,TT) :- number(T),tag_number(T,TT).

% string

tag_string(T,stringzero) :- assertion(\+blob(T,_)),string_length(T,0).
tag_string(T,string(T))  :- assertion(\+blob(T,_)),string_length(T,L),L>0.

% number: rational vs. float

tag_number(T,float(T)) :- float(T).
tag_number(T,TT)       :- rational(T,_,_),tag_rational(T,TT).

% rational: integer vs. "non-integer rational"

tag_rational(T,int(T))  :- integer(T).
tag_rational(T,prat(T)) :- \+integer(T).

% blob: atom vs. anything else

tag_blob(T,TT) :- atom(T),tag_atom(T,TT).
tag_blob(T,TT) :- \+atom(T),tag_nonatom_blob(T,TT).

% atom

tag_atom(T,atomzero) :- assertion(blob(T,text)),atom_length(T,0).
tag_atom(T,atom(T))  :- assertion(blob(T,text)),atom_length(T,L),L>0.

% nonatom blob

tag_nonatom_blob(T,TT) :- blob(T,reserved_symbol),tag_reserved_symbol_blob(T,TT).
tag_nonatom_blob(T,TT) :- \+blob(T,reserved_symbol),tag_foreign_resource_blob(T,TT).

% reserved symbol blob

tag_reserved_symbol_blob([],emptylist).
tag_reserved_symbol_blob(F,dictfunctor) :- dict_functor(F).

% foreign resource blob

tag_foreign_resource_blob(T,foreign(M,T)) :- blob(T,M).

% compound term, arbitrary arity

tag_compound(T,TT) :- compound_name_arity(T,_,0),tag_zero_arity_compound(T,TT).
tag_compound(T,TT) :- compound_name_arity(T,_,A),A>0,tag_nonzero_arity_compound(T,TT).

% compound term, zero arity

tag_zero_arity_compound(T,compzero(N)) :- compound_name_arity(T,N,0).

% compound term, nonzero arity, dict

tag_nonzero_arity_compound(T,dict(DictTag,Marks,TgDictArgs)) :-
   dict_functor(F),
   compound_name_arguments(T,F,[DictTag|DictArgs]),
   marks(T,Marks),
   re_pair(DictArgs,TgDictArgsUnsorted),
   keysort(TgDictArgsUnsorted,TgDictArgs).

% compound term, arity 2, listbox

tag_nonzero_arity_compound(T,lbox(Marks,TgHead,TgTail)) :-
   compound_name_arguments(T,'[|]',[Head,Tail]),
   marks(T,Marks),
   tag(Head,TgHead),
   tag(Tail,TgTail).

% compound term, arity 2, conjunction

tag_nonzero_arity_compound(T,conj(Marks,TgL,TgR)) :-
   compound_name_arguments(T,',',[L,R]),
   marks(T,Marks),
   tag(L,TgL),
   tag(R,TgR).

% compound term, arity 2, pair

tag_nonzero_arity_compound(T,pair(Marks,TgL,TgR)) :-
   compound_name_arguments(T,'-',[L,R]),
   marks(T,Marks),
   tag(L,TgL),
   tag(R,TgR).

% compound term, none of the above

tag_nonzero_arity_compound(T,compound(N,Marks,TgParams)) :-
   compound_name_arguments(T,N,Params),
   length(Params,L),
   \+ (dict_functor(N),L#=2*_+1), % would be a dict
   [N,L] \== ['[|]',2],          % would be a listbox
   [N,L] \== [',',2],            % would be a conjunction
   [N,L] \== ['-',2],            % would be a pair 
   marks(T,Marks),
   maplist(tag,Params,TgParams).

% Helper to associate keys and values of a dict

re_pair([],[]).

re_pair([V,K|VKs],[TgK-TgV|TgVKs]) :-
   tag(V,TgV),
   tag(K,TgK),
   re_pair(VKs,TgVKs).

% Helper to examine compound term substructure

marks(T,M) :- 
   M0=[],
   (cyclic_term(T) -> M1=[cyclic|M0] ; M1=M0),
   (ground(T)      -> M2=[gnd|M1]    ; M2=[nongnd|M1]),
   (is_list(T)     
    -> 
    M3=[list|M2]
    ; 
    compound_name_arity(T,'[|]',2)
    ->
    M3=[nonlist|M2]
    ;
    M3=M2),
   M=M3.

