:- use_module(library('heavycarbon/utils/clashfree_id_selection.pl')).
% :- use_module(library('heavycarbon/strings/string_of_spaces.pl')).

const(id_range,1000,9999).





% ============================================================================
% Given a term denoted by "Root", set up an SWI-Prolog "Dict" as follows:
% In that Dict, for each node of the term graph rooted at "Root" (i.e. for
% each compound, atomic or hole node of the graph structure),
% a "mirror representation" with meta-information can be found as a value,
% with a numeric Id as key. The numeric Id of the "Root" is returned in "Id".
% In effect, the term graph, a reference-based structure (as far as we know) 
% is transformed into an explicit name-based structure (with numeric Ids as
% names).
% ============================================================================

termgraph_mirror(Root,DictOut,Id) :-
   assertion(var(Root);nonvar(Root)),
   assertion(var(DictOut)),
   assertion(var(Id)),
   termgraph_mirror_2(Root,0,mirror{},DictOut,Id). % Depth is 0 at first

% ---
% Case of: The "root" is a "hole" that has already been seen
% ---

termgraph_mirror_2(Root,Depth,Dict0,Dict1,Id) :-
   var(Root),
   find_hole(Dict0,Root,Id),
   !,
   Mirror0 = Dict0.get(Id),
   fix_depth_list_of_hole(Depth,Mirror0,Mirror1),
   Dict1 = Dict0.put(Id,Mirror1).

% ---
% Case of: The "root" is a "hole" that has not yet been seen
% ---

termgraph_mirror_2(Root,Depth,Dict0,Dict1,Id) :-
   var(Root),
   \+find_hole(Dict0,Root,_),
   !,
   enlarge_dict_with_hole(Root,Depth,Dict0,Dict1,Id).

% ---
% Case of: The "root" is atomic (and thus a leaf).
% At this point, we don't care whether there already is a mirrored entry.
% This case includes: numbers (test number/1), strings (test string/1)
% atoms (test blob(T,text)), reserved symbols like the empty list (test
% blob(T,reserved_symbol)), other blobs. What do to with the "other blobs"?
% ---

termgraph_mirror_2(Root,Depth,Dict0,Dict1,Id) :-
   atomic(Root),!,
   enlarge_dict_with_atomic(Root,Depth,Dict0,Dict1,Id).

% ---
% Case of: The "root" is compound but not a dict
% ---

termgraph_mirror_2(Root,Depth,Dict0,Dict1,Id) :-
   compound(Root),\+is_dict(Root),!,
   compound_name_arguments(Root,Name,Args),
   % The child nodes may include *this very node, Root* (case of a cycle of length 1) 
   % So we have to upgrade the dict twice: once with an incomplete entry
   % because the child nodes are not known yet; once with a complete entry
   % once the child nodes are all known.
   % OTOH, we can't detect the cycle yet, so it's for naught :-(
   enlarge_dict_with_compound(Root,Name,Depth,Dict0,Dict00,Id), % add incomplete node
   succ(Depth,Depther),
   termgraph_mirror_children(Args,Depther,Dict00,Dict1,IdsOfArgs),
   fix_compound_mirror_in_place(Id,Dict1,IdsOfArgs).

% ---
% Case of: The "root" is a dict (and thus also compound, at least in the
% current implementation, but is best decomposed with dict_pairs/3.
% ---

termgraph_mirror_2(Root,Depth,Dict0,Dict1,Id) :-
   is_dict(Root),!,
   dict_pairs(Root,DictTag,Pairs),
   enlarge_dict_with_dict(Root,DictTag,Depth,Dict0,Dict00,Id), % add incomplete node
   succ(Depth,Depther),
   unzip_pairs(Pairs,Keys,Values),
   termgraph_mirror_children(Keys,Depther,Dict00,Dict01,IdsOfKeys),
   termgraph_mirror_children(Values,Depther,Dict01,Dict1,IdsOfValues),
   fix_dict_mirror_in_place(Id,Dict1,IdsOfKeys,IdsOfValues).

unzip_pairs(Pairs,Keys,Values) :-
   maplist([K-V,K,V]>>true,Pairs,Keys,Values).

% ---
% Process child nodes of a compound node or a dict
% ---

termgraph_mirror_children([Child|Children],Depth,Dict0,Dict1,[Id|Ids]) :-
   termgraph_mirror_2(Child,Depth,Dict0,Dict00,Id),
   termgraph_mirror_children(Children,Depth,Dict00,Dict1,Ids).

termgraph_mirror_children([],_,Dict,Dict,[]).



 
   
   
 
% ===
% These know about the structure of the mirror nodes
% ===

fix_depth_list_of_hole(Depth,hole(X,Id,L0),hole(X,Id,L1)) :-
   msort([Depth|L0],L1).

find_hole(Dict,X,Id) :-
   assertion(var(X)),
   % Decorate with "once" to stop at the first (and unique) element.
   % bagof/3 fails if there is no solution, which is what we want!
   bagof(IdFound,once(hole_finder(Dict,X,IdFound)),Bag),
   Bag=[Id].

hole_finder(Dict,X,Id) :-
   get_dict(Id,Dict,hole(XX,Id,_)),
   X==XX.
   
% ===
% Dict enlargement
% ===

% Add a mirror value for a "hole" (store reference to the "hole")

enlarge_dict_with_hole(X,Depth,Dict0,Dict1,Id) :-
   assertion(var(X)),
   const(id_range,MinId,MaxId),
   Mirror=hole(X,Id,[Depth]),
   insert_with_clashfree_id(Dict0,Mirror,Dict1,Id,MinId,MaxId).

% Add a mirror value for an atomic term (store reference - or value of - the term)

enlarge_dict_with_atomic(A,Depth,Dict0,Dict1,Id) :-
   assertion(atomic(A)),
   const(id_range,MinId,MaxId),
   Mirror=atomic(A,Id,Depth,meta{}), % Id is fresh here
   insert_with_clashfree_id(Dict0,Mirror,Dict1,Id,MinId,MaxId).

% Add a mirror value for a compound term; however it is incomplete: there is just a
% freshvar where the actual ids of the mirrored children will be

enlarge_dict_with_compound(C,Name,Depth,Dict0,Dict1,Id) :-
   assertion(compound(C)),
   const(id_range,MinId,MaxId),
   Mirror=compound(Id,Depth,Name,_,meta{}), % Id is fresh here; Ids of children left unspecified
   insert_with_clashfree_id(Dict0,Mirror,Dict1,Id,MinId,MaxId).

% Fix the mirror value for a compound term with the list of the known ids of the children.
% We just refine the fresh variable at the position of the "list of child ids", no
% setting involved.

fix_compound_mirror_in_place(Id,Dict,IdsOfArgs) :-
   compound(Id,_Depth,_Name,IdsOfArgs,_M) = Dict.get_dict(Id).

% Add a mirror value for a dict; however it is incomplete: there is just a
% freshvar where the actual ids of the mirrored keys and values will be

enlarge_dict_with_dict(D,DictTag,Depth,Dict0,Dict1,Id) :-
   assertion(is_dict(D)),
   const(id_range,MinId,MaxId),
   Mirror=dict(Id,Depth,DictTag,_,_,meta{}), % Id is fresh here; Ids of key/values left unspecified
   insert_with_clashfree_id(Dict0,Mirror,Dict1,Id,MinId,MaxId).

% Fix the mirror value for a dict  with the list of the known ids of the keys
% and values. We just refine the fresh variables at the positions of the
% "list of key/value ids", no setting involved.

fix_dict_mirror_in_place(Id,Dict,IdsOfKeys,IdsOfValues) :-
   dict(Id,_Depth,_DictTag,IdsOfKeys,IdsOfValues,_M) = Dict.get_dict(Id).

