% ===
% Transform a database-based tree description into a term
% ===

:- use_module(library('snippets/prettyprint_dict.pl')). 
:- use_module(library('heavycarbon/support/meta_helpers.pl')).
:- use_module(library(clpfd)).

% ============================
% Tree described by facts: node(?Name,?Parent).
% ============================

node(a,[]).
node(b,a).
node(c,a).
node(d,b).
node(e,b).
node(f,c).
node(g,c).
node(h,d).
node(i,d).
node(j,e).
node(k,e).
node(l,f).
node(m,f).
node(n,g).
node(o,g).

% ===
% Special key for Container
% ===

root_entry_key('_root_').

build_term(Container2) :-
   bagof(Name,Parent^node(Name,Parent),Names),  % collect all node names; fails if there are none
   node_container_from_names(Names,node_container,Container1,RootName),
   uptick_dict_with_root_entry(RootName,Container1,Container2),
   patch_depth_entries(Container2), 
   prettyprint_dict(Container2).

% ---
% Fill in the still-unbound values of the "depth" entries. No modified dicts have to 
% be created. While doing so, also collect the names of the leaf nodes. It is difficult to
% judge how this would work on "large graphs" (hundreds of nodes? how many can an SWI-Prolog
% dict handle?). The question of "find me all the nodes whose parent is X" needs to be performed
% by scanning the whole dict with bagof/3. How would an index that allows this to be done
% quickly look like? It would be an AVL tree on the side, basically.
% ---

patch_depth_entries(Container) :-
   root_name_and_node(Container,_,RootNode),
   get_dict(depth,RootNode,0),                            % this unifies the the 'depth' value of RootNode to 0 (first patch)
   dict_keys(Container,NodeNames),                        % might be interesting to generate this in a coroutine?
   maplist(patch_depth_entries_2(Container),NodeNames),   % as we patch "in place" (the pseudo-side-effect is "further instantiation of a term"), maplist can be used: clean code!
   maplist(patch_unbound_leaf_flags(Container),NodeNames). % the "leaf flags" of leaf nodes are still unbound

% ---
% 
% ---

patch_unbound_leaf_flags(_,NodeName) :-
   root_entry_key(NodeName),  % NodeName = REK?
   !.                         % just skip this!
 
patch_unbound_leaf_flags(Container,NodeName) :-
   get_dict(NodeName,Container,Node),
   (get_dict(leaf,Node,true) -> true ; true).  % On succes, it was patched. On failure, it was already patched.
 
% ---
% As called by maplist with the name of a node: find its depth, i.e. its distance from the root
% ---

patch_depth_entries_2(_,NodeName) :-
   root_entry_key(NodeName),  % NodeName = REK?
   !.                         % just skip this!
 
patch_depth_entries_2(Container,NodeName) :-
   patch_rootwards(Container,NodeName,0,_).   % recursively patch nodes while "moving towards root"
   
% ---
% Recursing down the child->parent links towards the root,
% and patching the "depth values" on all nodes on that path
% and the "leaf flag" values on all nodes except the first on that path
% on return. This is actually a fold-right which just does the path
% collection and patching simultaneously. Implementation-wise, we
% make this a tail-call (seemingly) by setting up a constraint between
% the (known later) depth of a parent node and the unknown depth of
% a current node.
% "DepthOfChild" is an unbound variable that will participate in 
% the constraint (and become a known value when we find a node
% with a known depth)
% "Distance" is the distance from the bottommost node (could also
% be a flag saying "I am the bottommost") It is used to decide
% whether to patch the "leaf flag" to "false".
% ---

patch_rootwards(Container,NodeName,Distance,DepthOfChild) :-
   assertion(var(DepthOfChild)),
   get_dict(NodeName,Container,Node),
   get_dict(depth,Node,Depth),                % Depth may or may not have been set already
   DepthOfChild #= Depth+1,                   % exchange non-tail recursion against the cost of setting up constraints (is it profitable?)
   if_then_else(
      nonvar(Depth),                          % already patched (which is the case for the root at least)
      (assertion(Depth>=0),                   % end of recursion! do nothing except patching the "leaf" flag (which may do nothing)
       patch_leaf_flag(Distance,Node)),       % if Distance > 0, patch the leaf "flag" to "false"
      (get_dict(parent,Node,ParentName),       
       succ(Distance,DistancePlus),
       patch_rootwards(Container,ParentName,DistancePlus,Depth))).  % due to the #= we don't need to compute "Depth" from "DepthOfParent" after return
       
% ---
% Always patch the "leaf flag" to "false" except if 
% Distance is actually still 0
% ---

patch_leaf_flag(0,_Node) :- !.      % Do nothing; this may not be a leaf!

patch_leaf_flag(Distance,Node) :-
   Distance > 0,
   get_dict(leaf,Node,false).      % unifies the the 'leaf' flag value of Node to false
   
% ---
% Add the entry for "_root_" to the container.
% ---

uptick_dict_with_root_entry(RootName,ContainerIn,ContainerOut) :-
   root_entry_key(REK),
   if_then(get_dict(REK,ContainerIn,_),fail), % entry for root already exists -> fail!
   put_dict(REK,ContainerIn,RootName,ContainerOut).

% ---
% Building the node container. "RootName" is unified with the name of the
% unique node which has no parent.
% ---

node_container_from_names(Names,Tag,Container,RootName) :-
   dict_create(StartContainer,Tag,[]),
   foldl(uptick_node_container(RootName),Names,StartContainer,Container).

uptick_node_container(RootName,Name,ContainerIn,ContainerOut) :-
   atomic(Name),       % name must be atomic otherwise it can't be used as key in a dict   
   node(Name,Parent),  % as given by the database (there should be only one!)
   switch(
      (Parent == [],var(RootName)),RootName=Name   % found the root! retain its name
      ,
      (Parent == [],nonvar(RootName)),fail         % dual root detected -> fail!
      ,
      true
   ),
   if_then(get_dict(Name,ContainerIn,_),fail), % dual name detected -> fail!
   put_dict(
      Name,
      ContainerIn,
      node{ name:Name,            
            parent:Parent,
            leaf:_,      % an unbound variable that will take up the "leaf" flag (true, false)
            depth:_ },      % an unbound variable that will take up the "depth"
      ContainerOut).

% ---
% Helpers
% ---

root_name_and_node(Container,RootName,RootNode) :-
   root_entry_key(REK),
   get_dict(REK,Container,RootName),       % fails or RootName is the name of the Root Node
   get_dict(RootName,Container,RootNode).  % get the RootNode out of the Container, too



