:- module(heavycarbon_terms,
          [
             mirror/3 % mirror(Node,Id,acc(MDi,MDo))
          ]).

:- use_module(library('heavycarbon/utils/clashfree_id_selection.pl')).
:- include(library('heavycarbon/support/throwme_nonmodular.pl')).
:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).

% :- use_module(library('heavycarbon/strings/string_of_spaces.pl')).

const(id_range,1000,9999).

% To be added: Find cycles using
% https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

% ---
% Aliases for var/1 and nonvar/1. Are the names of these less confusing?
% I hope so. var/1 and nonvar/1 are really badly chosen.
% TODO: export into another module
% ---

bound(X)   :- nonvar(X).
unbound(X) :- var(X).

% ---
% A modified "insert with clashfree id" with hardcoded limits
% ---

insert_with_clashfree_id(Id-MirrorTerm,acc(MDi,MDo)) :-
   const(id_range,MinId,MaxId),
   insert_with_clashfree_id(Id-MirrorTerm,acc(MDi,MDo),limit(MinId,MaxId)).

% ============================================================================
% Given a term denoted by "Node", set up an SWI-Prolog "Dict" called 
% "MirrorDict" as follows: 
%
% - For each node of the term graph rooted at "Node" (i.e. for each compound
%   node, atomic node or hole node of the graph structure), a "mirror 
%   representation" structure holding meta-information is added to
%   "MirrorDict". It is also a "Dict".
% - A randomly chosen numeric id is used to identify the mirror representation
%   in the "MirrorDict". That numeric is unified with "Id".
%
% In effect, the term graph, a reference-based structure (as far as we know,
% but implementations may differ) is transformed into an explicit name-based
% structure (with numeric ids as names). That structure is of course itself a
% term.
%
% This predicate could be working in both directions:
%
% - If Node and MirrorDict are bound on call and Id is not, map the term
%   graph rooted at Node into the name-based structure, using MirrorDict as
%   lookup structure, and unify the Id with the name of the mirror of Node.
% - If Node is unbound on call but MirrorDict and Id are, generate the term
%   graph mirrored in MirrorDict, starting at Id, and unify Node with the
%   result.
%
% For "hole nodes", do not create a new "hole" to mirror the existing one;
% reuse the "hole node" (this opens up some interesting possibilities for
% later, actually)
%
% MirrorDict works as accumulator, so comes in a I/O pair
%
% The accumulator "Mirror Dict In" ----> "Mirror Dict Out" is given by
% the argument "acc(MDi,MDo)", which turns out to be rather readable!
% ============================================================================

mirror(Node,Id,acc(MDi,MDo)) :-
   must_be(nonvar,MDi), 
   must_be(var,MDo),
   switch(
      bound(Id),     (mirror_to_node(Id,Node,MDi),MDo=MDi),
      unbound(Id),   node_to_mirror(Node,Id,acc(MDi,MDo))).

% ---
% Id must be bound, and names the root mirror node found in the MD mirror node dictionary.
% The mirror node dictionary does not change
% ---

mirror_to_node(Id,Node,MD) :-
   assertion(bound(Id)),                    % Id is known
   get_dict(Id,MD,MirrorTerm),              % Retrieve (fails if unknown)
   map_mirror_term(What,Args,MirrorTerm),   % Analyze into What + Args 
   mirror_term_to_node(What,Args,Node0,MD), % This will perform recursively, children first
   Node=Node0.                              % Depending on whether Node is bound/unbound, this is a test or a return

% ---
% Building a "new node" from a known mirror term
% ---
   
mirror_term_to_node(hole,[_,Node],Node,_)   :- !. % done at once, just shunt stored Node into NewNode
mirror_term_to_node(atomic,[_,Node],Node,_) :- !. % done at once, just shunt stored Node into NewNode

mirror_term_to_node(dict,[_,DictTag,KeyIds,ValueIds],DictNode,MD) :-
   unmirror_children(KeyIds,KeyNodes,MD),
   unmirror_children(ValueIds,ValueNodes,MD),
   pair_zipping(DictPairs,KeyNodes,ValueNodes),
   dict_pairs(DictNode,DictTag,DictPairs).

mirror_term_to_node(compound,[_,Name,Children],CompoundNode,MD) :- 
   unmirror_children(Children,ChildNodes,MD),
   compound_name_arguments(CompoundNode,Name,ChildNodes).

% ---
% Process child nodes of a compound node (or a dict, which is also a compound in the present
% implementation)
% ---

unmirror_children([],[],_).

unmirror_children([Id|Ids],[Node|Nodes],MD) :-
   mirror_to_node(Id,Node,MD),
   unmirror_children(Ids,Nodes,MD).

% ---
% Id must be unbound, and will be refined to the id of the "mirror node" mirroring "Node".
% Node unbound : We assume this is a "hole node".
% Node bound   : We assume this is a "root node" of a "term tree" to be mirrored.
% The mirror node dictionary is transformed from MDi to MDo (accumulator approach).
% ---
 
node_to_mirror(Node,Id,acc(MDi,MDo)) :-
   assertion(unbound(Id)),
   switch(
      unbound(Node),  unbound_node_to_mirror(Node,Id,acc(MDi,MDo)),    % Node is leaf of "term tree"
      atomic(Node),   atomic_node_to_mirror(Node,Id,acc(MDi,MDo)),     % Node is leaf of "term tree"
      is_dict(Node),  dict_node_to_mirror(Node,Id,acc(MDi,MDo)),       % Node is root node of "term tree"; call is recursive
      compound(Node), compound_node_to_mirror(Node,Id,acc(MDi,MDo))).  % Node is root node of "term tree"; call is recursive

% ---
% Creating the mirror terms; this establishes relationships between
% the mirror term's structure (it is compound) and individual values 
% found in that compound term.
% ---

             %  Select     Args                           Mirror compound term
map_mirror_term(hole     , [Id,Node]                    , hole(Id,Node)).
map_mirror_term(dict     , [Id,DictTag,KeyIds,ValueIds] , dict(Id,DictTag,KeyIds,ValueIds,meta{})). % no nee to retain Node
map_mirror_term(atomic   , [Id,Node]                    , atomic(Id,Node)).
map_mirror_term(compound , [Id,Name,Children]           , compound(Id,Name,Children)).
 
% ---
% Mirroring an "unbound" node. Make sure it exists only once in the mirror.
% ---

unbound_node_to_mirror(Node,Id,acc(MD,MD)) :-
   find_hole(MD,Node,Id),     % if this succeeds, the "var" node has already been seen
   !.                         % do nothing 

unbound_node_to_mirror(Node,Id,acc(MDi,MDo)) :-
   \+find_hole(MDi,Node,Id),   % if this succeeds, "the" var node has not yet been seen
   !,                      
   map_mirror_term(hole,[Id,Node],MirrorTerm),
   insert_with_clashfree_id(Id-MirrorTerm,acc(MDi,MDo)).

find_hole(MD,Node,Id) :-
   assertion(var(Node)),
   % Decorate with "once" to stop at the first (and unique) element.
   % bagof/3 fails if there is no solution, which is what we want!
   bagof(IdFound,once(hole_finder(MD,Node,IdFound)),[Id]).

hole_finder(MD,Node,Id) :-
   get_dict(Id,MD,hole(X,Id,_)),
   X==Node.
 
% ---
% Mirroring an "atomic" node. Don't care about duplicates
% ---

atomic_node_to_mirror(Node,Id,acc(MDi,MDo)) :-
   map_mirror_term(atomic,[Id,Node],MirrorTerm),
   insert_with_clashfree_id(Id-MirrorTerm,acc(MDi,MDo)).

% ---
% Mirroring a "dict" node, which is a special compound node.
% ---

dict_node_to_mirror(Node,Id,acc(MDi,MDo)) :-
   dict_pairs(Node,DictTag,DictPairs0),
   keysort(DictPairs0,DictPairs1), % TODO possibly do not sort depending on flag
   incompletely_mirror_some_dict(Id,DictTag,acc(MDi,MD0)),
   pair_zipping(DictPairs1,KeyNodes,ValueNodes),
   mirror_children(KeyNodes,KeyIds,acc(MD0,MD1)),
   mirror_children(ValueNodes,ValueIds,acc(MD1,MDo)),
   fixup_dict_mirror(Id,MDo,KeyIds,ValueIds).

% ---
% Fix the mirror value for a dict with the list of the (now) known ids of the 
% keys and values. We just refine the fresh variables at the corresponding 
% places of the compound term that is the mirror value of the dict. There
% is no need to change the lookup dict holding that mirror value.
% ---

fixup_dict_mirror(Id,MD,KeyIds,ValueIds) :-
   get_dict(Id,MD,MirrorTerm),                              % Retrieve
   map_mirror_term(dict,[Id,_,KeyIds,ValueIds],MirrorTerm). % Unify in place

% ---
% Add a mirror value for a dict; however it is incomplete: there is just a
% unbound variable where the actual ids of the mirrored keys and values will be!
% Note that we don't care to retain the original "Node", we have all the info
% in the mirror structure!
% ---

incompletely_mirror_some_dict(Id,DictTag,acc(MDi,MDo)) :-
   map_mirror_term(dict,[Id,DictTag,_,_],MirrorTerm),
   insert_with_clashfree_id(Id-MirrorTerm,acc(MDi,MDo)).

% ---
% zip/unzip pairs in one predicate
% ---

pair_zipping(Pairs,Keys,Values) :-
   maplist([K-V,K,V]>>true,Pairs,Keys,Values).

% ---
% Process child nodes of a compound node (or a dict, which is also a compound in the present
% implementation)
% ---

mirror_children([],[],acc(MD,MD)).

mirror_children([Node|Nodes],[Id|Ids],acc(MDi,MDo)) :-
   node_to_mirror(Node,Id,acc(MDi,MD0)),
   mirror_children(Nodes,Ids,acc(MD0,MDo)).

% ---
% Mirroring a compound node
% The child nodes may include *this very node* (case of a cycle of length 1) 
% So we have to upgrade the dict twice: once with an incomplete entry
% because the child nodes are not known yet; once with a complete entry
% once the child nodes are all known.
% OTOH, we can't detect the cycle yet, so it's for naught :-(
% ---

compound_node_to_mirror(Node,Id,acc(MDi,MDo)) :-
   compound_name_arguments(Node,Name,ChildNodes),
   incompletely_mirror_some_compound(Id,Name,acc(MDi,MD0)), 
   mirror_children(ChildNodes,ChildIds,acc(MD0,MDo)),
   fixup_compound_mirror(Id,MDo,ChildIds).

% ---
% Add a mirror value for a compound; however it is incomplete: there is just a
% unbound variable where the actual ids of the mirrored children will be!
% Note that we don't care to retain the original "Node", we have all the info
% in the mirror structure!
% ---

incompletely_mirror_some_compound(Id,Name,acc(MDi,MDo)) :-
   map_mirror_term(compound,[Id,Name,_],MirrorTerm),
   insert_with_clashfree_id(Id-MirrorTerm,acc(MDi,MDo)).

% ---
% Fix the mirror value for a compound with the list of the (now) known ids of the 
% children. We just refine the fresh variable at the corresponding 
% place of the compound term that is the mirror value of the compound. There
% is no need to change the lookup dict holding that mirror value.
% ---

fixup_compound_mirror(Id,MD,ChildIds) :-
   get_dict(Id,MD,MirrorTerm),                           % Retrieve
   map_mirror_term(compound,[Id,_,ChildIds],MirrorTerm). % Unify in place

% ===
% For testing, create atoms of random length
% ===

the_list(List,Max) :- 
   atom_chars('abcdefghijklmnopqrstuvwxyz',List),
   length(List,Length),
   succ(Max,Length).

random_char_list([X|More],AlreadyGot) :-
   the_list(List,Max),
   random_between(0,Max,N),
   nth0(N,List,X),
   CutValue is 1-(1/(1+exp(2-(AlreadyGot/2)))), % nice sigmoid expressing decreasing probability of continuing, plot it at https://www.desmos.com/calculator
   ((random(F),F<CutValue)
    -> (succ(AlreadyGot,AlreadyGotNow),random_char_list(More,AlreadyGotNow)) 
    ;  More=[]).

random_atom(Atom) :- 
   random_char_list(Chars,0),
   atom_chars(Atom,Chars).


% ===
% Nice sigmoid expressing decreasing probability of continuing with one
% more character given there already are L characters.
% Plot it at https://www.desmos.com/calculator
% ===

sigmoid_value(L,P) :- P is 1-(1/(1+exp(2-(L/2)))).
 
% ===
% For testing, create a preliminary tree by injecting N nodes into a tree with arbitrary
% branching and depth. 
% == 

insert_randomly(Element,List,ListNew) :-
   length(List,L),
   random_between(0,L,Pos),
   length(Left,Pos),
   append(Left,Right,List),
   append([Left,[Element],Right],ListNew).
   

preliminary_tree_inject(Depth,acc(NodeIn,NodeOut)) :-
   assertion(bound(NodeIn)),

   NodeIn = node(Children),

   % either insert here or insert lower
   sigmoid_value(D,P),
   random(Toss),                % Between 0.0 =< Toss < 1.0
   ((P<Toss)                    % Insert here if Toss is above P
    -> 
    randomly_insert(Children,ChildrenNew),
    NodeOut=node(ChildrenNew)
    ;
    randomly_select(


   
   


   N = node(Children)
   


/*
             %  Select     Args                           Mirror compound term
map_mirror_term(hole     , [Id,Node]                    , hole(Id,Node)).
map_mirror_term(dict     , [Id,DictTag,KeyIds,ValueIds] , dict(Id,DictTag,KeyIds,ValueIds,meta{})). % no nee to retain Node
map_mirror_term(atomic   , [Id,Node]                    , atomic(Id,Node)).
map_mirror_term(compound , [Id,Name,Children]           , compound(Id,Name,Children)).
*/

/*      
fix_depth_list_of_hole(Depth,hole(X,Id,L0),hole(X,Id,L1)) :-
   msort([Depth|L0],L1).

% ===
% Exception descriptors
% ===

exc_desc(unmirror,no_entry_for_id(Id),
         _,
         unmirror(Id),
         'no entry in mirror dict for indicated id').

exc_desc(unmirror,unmirroring_failure(Mirror,Id),
         _,
         unmirror(Mirror,Id),
         'generic unmirroring failure').

*/
