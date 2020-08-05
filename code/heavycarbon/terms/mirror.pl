:- module(heavycarbon_terms,
          [
              unmirror/3   % unmirror(+Id,+MirrorDict,?Node)
             ,mirror/3     % mirror(+Node,-MirrorDict,-Id)
          ]).

:- use_module(library('heavycarbon/utils/clashfree_id_selection.pl')).
:- include(library('heavycarbon/support/throwme_nonmodular.pl')).
:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).

% :- use_module(library('heavycarbon/strings/string_of_spaces.pl')).

const(id_range,1000,9999).

% To be added: Find cycles using
% https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

% ===
% Generate a random term
% ===

%%% TO BE DONE

% ============================================================================
% Regenerate a term from its MirrorDict, the term being rooted at "Id"
% ============================================================================

% In the "ideal Prolog", this would not need to exist; one would just
% read the mirroring code "backwards". A big problem comes from the 
% undeterminism of "randonmly selected ids".

unmirror(Id,MirrorDict,Node) :-
   assertion(integer(Id)),
   assertion(is_dict(MirrorDict)),
   unmirror_2(Id,MirrorDict,NodeX),
   Node = NodeX.
 
unmirror_2(Id,MiDict,Node) :-
   unless(get_dict(Id,MiDict,MirrorNode),throwme(unmirror,no_entry_for_id(Id))),
   unless(unmirror_3(MirrorNode,MiDict,Id,Node),throwme(unmirror,unmirroring_failure(MirrorNode,Id))).

% unmirror_3 also checks that the Id is indeed in the Mirror term and fials if not

unmirror_3(hole(NodeX,Id,_D),_MiDict,Id,NodeX).

unmirror_3(atomic(NodeA,Id,_D,_M),_MiDict,Id,NodeA).

unmirror_3(compound(Id,_D,Name,ChildIds,_M),MiDict,Id,NodeC) :-
   unmirror_children(ChildIds,ChildNodes,MiDict),
   compound_name_arguments(NodeC,Name,ChildNodes).

unmirror_3(dict(Id,_D,DictTag,KeyIds,ValueIds,_M),MiDict,Id,NodeD) :-
   unmirror_children(KeyIds,KeyNodes,MiDict),
   unmirror_children(ValueIds,ValueNodes,MiDict),
   pair_zipping(DictPairs,KeyNodes,ValueNodes),
   dict_pairs(NodeD,DictTag,DictPairs).
  
unmirror_children([Id|Ids],[Node|Nodes],MiDict) :-
   unmirror(Id,MiDict,Node),
   unmirror_children(Ids,Nodes,MiDict).

unmirror_children([],[],_).

% ============================================================================
% Given a term denoted by "Node", set up an SWI-Prolog Dict called 
% "MirrorDict" as follows: 
% For each node of the term graph rooted at "Root" (i.e. for each compound,
% atomic or hole node of the graph structure), a "mirror representation" with
% meta-information can be found as a value, with a randomly chosen numeric id
% as key. The numeric Id of the "Node" is returned in "Id".
% In effect, the term graph, a reference-based structure (as far as we know,
% but implementations may differ) is transformed into an explicit name-based
% structure (with numeric Ids as names), which is itself a term.
% ============================================================================

mirror(Node,MirrorDict,Id) :-
   assertion(var(MirrorDict)),
   assertion(var(Id)),
   mirror_2(Node,0,mirror{},MirrorDict,Id). % 0 depth at first

% ---
% Case of: "node" is a "hole" that has already been seen
% ---

mirror_2(Node,Depth,MiDict0,MiDict1,Id) :-
   var(Node),
   find_hole(MiDict0,Node,Id),
   !,
   MiNode0 = MiDict0.get(Id),
   fix_depth_list_of_hole(Depth,MiNode0,MiNode1),
   MiDict1 = MiDict0.put(Id,MiNode1). % TODO: Replace with a difflist op

% ---
% Case of: "node" is a "hole" that has not yet been seen
% ---

mirror_2(Node,Depth,MiDict0,MiDict1,Id) :-
   var(Node),
   \+find_hole(MiDict0,Node,_),
   !,
   mirror_hole(Node,Depth,MiDict0,MiDict1,Id).

% ---
% Case of: The "node" is atomic (and thus a leaf).
% At this point, we don't care whether there already is a mirrored entry with
% the same "atomic content" (in structures with high redundance, this could be of
% interest)
% This case includes: numbers (test number/1), strings (test string/1)
% atoms (test blob(T,text)), reserved symbols like the empty list (test
% blob(T,reserved_symbol)), other blobs. What do to with the "other blobs" though?
% ---

mirror_2(Node,Depth,MiDict0,MiDict1,Id) :-
   atomic(Node),
   !,
   mirror_atomic(Node,Depth,MiDict0,MiDict1,Id).

% ---
% Case of: The "node" is compound but not a dict
% ---

mirror_2(Node,Depth,MiDict0,MiDict1,Id) :-
   compound(Node),\+is_dict(Node),
   !,
   compound_name_arguments(Node,Name,ChildNodes),
   % The child nodes may include *this very node* (case of a cycle of length 1) 
   % So we have to upgrade the dict twice: once with an incomplete entry
   % because the child nodes are not known yet; once with a complete entry
   % once the child nodes are all known.
   % OTOH, we can't detect the cycle yet, so it's for naught :-(
   mirror_compound(Node,Name,Depth,MiDict0,MiDict00,Id), % add incomplete node
   succ(Depth,Depther),
   mirror_children(ChildNodes,Depther,MiDict00,MiDict1,ChildIds),
   fix_compound_mirror_in_place(Id,MiDict1,ChildIds).

% ---
% Case of: The "node" is a dict (and thus also compound, at least in the
% current implementation); best decomposed with dict_pairs/3.
% ---

mirror_2(Node,Depth,MiDict0,MiDict1,Id) :-
   is_dict(Node),
   !,
   dict_pairs(Node,DictTag,DictPairs),
   mirror_dict(Node,DictTag,Depth,MiDict0,MiDict00,Id), % add incomplete node
   succ(Depth,Depther),
   pair_zipping(DictPairs,KeyNodes,ValueNodes),
   mirror_children(KeyNodes,Depther,MiDict00,MiDict01,KeyIds),
   mirror_children(ValueNodes,Depther,MiDict01,MiDict1,ValueIds),
   fix_dict_mirror_in_place(Id,MiDict1,KeyIds,ValueIds).

% ---
% zip/unzip in one predicate
% ---

pair_zipping(Pairs,Keys,Values) :-
   maplist([K-V,K,V]>>true,Pairs,Keys,Values).

% ---
% Process child nodes of a compound node or a dict
% ---

mirror_children([Node|Nodes],Depth,MiDict0,MiDict1,[Id|Ids]) :-
   mirror_2(Node,Depth,MiDict0,MiDict00,Id),
   mirror_children(Nodes,Depth,MiDict00,MiDict1,Ids).

mirror_children([],_,MiDict,MiDict,[]).

% ===
% The following predicates know about the structure of the mirror nodes
% ===

fix_depth_list_of_hole(Depth,hole(X,Id,L0),hole(X,Id,L1)) :-
   msort([Depth|L0],L1).

find_hole(MiDict,X,Id) :-
   assertion(var(X)),
   % Decorate with "once" to stop at the first (and unique) element.
   % bagof/3 fails if there is no solution, which is what we want!
   bagof(IdFound,once(hole_finder(MiDict,X,IdFound)),Bag),
   Bag=[Id].

hole_finder(MiDict,X,Id) :-
   get_dict(Id,MiDict,hole(XX,Id,_)),
   X==XX.
   
% Add a mirror value for a "hole" (store reference to the "hole")

mirror_hole(X,Depth,MiDict0,MiDict1,Id) :-
   assertion(var(X)),
   const(id_range,MinId,MaxId),
   MirrorNode=hole(X,Id,[Depth]),
   insert_with_clashfree_id(MiDict0,MirrorNode,MiDict1,Id,MinId,MaxId).

% Add a mirror value for an atomic term (store reference - or value of - the term)

mirror_atomic(A,Depth,MiDict0,MiDict1,Id) :-
   assertion(atomic(A)),
   const(id_range,MinId,MaxId),
   MirrorNode=atomic(A,Id,Depth,meta{}), % Id is fresh here
   insert_with_clashfree_id(MiDict0,MirrorNode,MiDict1,Id,MinId,MaxId).

% Add a mirror value for a compound term; however it is incomplete: there is just a
% freshvar where the actual ids of the mirrored children will be

mirror_compound(C,Name,Depth,MiDict0,MiDict1,Id) :-
   assertion(compound(C)),
   const(id_range,MinId,MaxId),
   MirrorNode=compound(Id,Depth,Name,_,meta{}), % Id is fresh here; Ids of children left unspecified
   insert_with_clashfree_id(MiDict0,MirrorNode,MiDict1,Id,MinId,MaxId).

% Fix the mirror value for a compound term with the list of the known ids of the children.
% We just refine the fresh variable at the position of the "list of child ids", no
% setting involved.

fix_compound_mirror_in_place(Id,MiDict,ChildIds) :-
   compound(Id,_Depth,_Name,ChildIds,_M) = MiDict.get_dict(Id).

% Add a mirror value for a dict; however it is incomplete: there is just a
% freshvar where the actual ids of the mirrored keys and values will be

mirror_dict(D,DictTag,Depth,MiDict0,MiDict1,Id) :-
   assertion(is_dict(D)),
   const(id_range,MinId,MaxId),
   MirrorNode=dict(Id,Depth,DictTag,_,_,meta{}), % Id is fresh here; Ids of key/values left unspecified
   insert_with_clashfree_id(MiDict0,MirrorNode,MiDict1,Id,MinId,MaxId).

% Fix the mirror value for a dict  with the list of the known ids of the keys
% and values. We just refine the fresh variables at the positions of the
% "list of key/value ids", no setting involved.

fix_dict_mirror_in_place(Id,MiDict,KeyIds,ValueIds) :-
   dict(Id,_Depth,_DictTag,KeyIds,ValueIds,_M) = MiDict.get_dict(Id).

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


