:- module(vector_replace0,
          [
             vector_replace0/4
          ]).

% ============================================================================
% 2020-04-XX
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
% Replace multiple elements in ListIn by giving a list of pairs
% Key-Value as ReplacePairs. Builds the list with replacements applied
% called ListOut and a list of pairs Key-OldValue of the prior values.
% 
%   vector_replace0(+ListIn,+ReplacePairs,?ListOut,=ReplacedPairs).
%
% The implementation deploys maplist, foldl and association lists (AVL trees)
% from library(assoc) to great advantage.
%
% This sort of replacement does not "destroy information" (a bijective mapping
% exists), so you can apply vector_replace0/4 a second time to obtain the
% original list again:
%
% IN=[a,b,c,d],
% INREP=[0-e,1-f,2-g,3-h],
% vector_replace0(IN,INREP,OUT,OUTREP),
% vector_replace0(OUT,OUTREP,IN,INREP).
% ============================================================================

vector_replace0(ListIn,ReplacePairs,ListOut,ReplacedPairs) :-
   maplist([_,_,_]>>true,ListIn,ListOut,Indexes),          % This "makes sure" that ListIn and ListOut are the same length and also creates Indexes
   foldl([Xcur,Xcur,Xnext]>>succ(Xcur,Xnext),Indexes,0,_), % Indexes is now a list [0,1,2,3,...] of the same length as ListIn
   debug(topic,"Indexes: ~q",[Indexes]),
   maplist([I,V,I-V]>>true,Indexes,ListIn,PairsIn),        % PairsIn is now a list [0-A,1-B-2-C,...] where A,B,C are the items of ListIn 
   debug(topic,"PairsIn: ~q",[PairsIn]),
   list_to_assoc(PairsIn,AssocIn),                         % AssocIn is now an AVL tree with keys the indexes and values the items of ListIn
   empty_assoc(ReplacementsAssocIn),                       % We will store replaced pairs in an assoc to check for duplicate replacements
   FoldlStarter = acc(AssocIn,ReplacementsAssocIn),        % Use a compound term for foldl's starter to pass TWO values
   FoldlResult  = acc(AssocOut,ReplacementsAssocOut),      % Pre-assemble the foldl result (could also be done after foldl)
   foldl(foldlize, 
         ReplacePairs,                                     % foldl iterates over the ReplacePairs list; no need to sort it by key, we replace as it comes
         FoldlStarter,                                     % accumulator, initial state
         FoldlResult),                                     % accumulator, final state
   assoc_to_values(AssocOut,ListOut),                      % Put the values from the AVL tree into the ListOut (it will be sorted by index)
   debug(topic,"ListOut: ~q",[ListOut]),
   assoc_to_list(ReplacementsAssocOut,ReplacedPairs),      % Put the pairs from the "replacements" into the ReplacedPairs
   debug(topic,"ReplacedPairs: ~q",[ReplacedPairs]).

% --- 
% For each K-V replacement pair, create a new assoc and 
% update the assoc of the replacements.
% ---

foldlize(K-V,
         acc(AssocIn,ReplacementsAssocIn),
         acc(AssocOut,ReplacementsAssocOut)) :- 
   debug(topic,"foldlize(~q,...)",[K-V]),
   (get_assoc(K,AssocIn,OldValue) 
    -> true
    ;  throw("Key does not exist in assoc!")),
   (get_assoc(K,ReplacementsAssocIn,_) 
    -> throw("Double replacement! Key already exists in replacement assoc!")
    ;  true),
   put_assoc(K,AssocIn,V,AssocOut),
   debug(topic,"Assoc: ~q --> ~q",[AssocIn,AssocOut]),
   put_assoc(K,ReplacementsAssocIn,OldValue,ReplacementsAssocOut),
   debug(topic,"ReplacementsAssoc: ~q --> ~q",[ReplacementsAssocIn,ReplacementsAssocOut]).

