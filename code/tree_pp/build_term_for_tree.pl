% ===
% Tree description
% ----------------
% 
% "A tree is a set of nodes linked by edges." This is the physical intuition.
% But how do we represent a tree?
% 
% Here is one way: Each node is designated by a unique 'name' (which is an 
% atom). Each edge is represented by a fact 
%
% edge(Name,Parent)
%
% where "Name" and "Parent" are node names and the Prolog fact expresses 
% the physical fact that node "Name" is a child of node "Parent". 
%
% We don't need to list the node names separately. The edge/2 facts directly
% imply the set of valid node names via:
%
% setof(Name,Parent^edge(Name,Parent),NodeNames).
%
% There is excatly one special node that has no parent. It is the "root node".
% It doesn't even need to appear in "Name" position in an edge/2 fact unless
% the tree consist only of the root node. We want to make sure that every
% node name appears in edge/2 parent position for ease-of-programming so
% the "synthetic" fact
%
% edge(root,[])
%
% will be used. In this case, the root node is named 'root'. It has no parent
% node but for programming's ease we pretend it does, and use the empty list
% as a stand-in.
%
% Indicating for each node the parent of that node describes a tree as long
% as:
%
% 1) the resulting graph is "fully connected", i.e. each node can be reached 
%    from any other node (you are allowed to go from child to parent or from
%    parent to child)
% 2) there are no cycles in the resulting graph; a little reflection will
%    tell you that, given that each node can only have a single parent,
%    the graph is fully connected and the existence of a single
%    parentless root node implies that the graph does not have cycles.
%
% So to know whether a set of edge/2 facts indeed describes a tree, we need
% to ascertain that 
%
% 1) There is a root node (can be done by inspection over edge/2)
% 2) The root node is unique (can be done by inspection over edge/2)
% 3) Each node only has a single parent (edge(N,P1),edge(N,P2),P1\==P2 is
%    not allowed) (can be done by inspection over edge/2)
% 4) The graph is fully connected (once could trace the path to the root
%    node for every node or check connectedness later; let's leave that
%    decision based on what comes easiest when building the algorithm)
% ===

:- module(build,
          [
          build_term/1
          ]).

:- use_module(library('heavycarbon/support/meta_helpers.pl')).
:- use_module(library(aggregate)).
:- use_module(library(clpfd)).


% ===
% edge(Name,Parent)
% "Name"   is the name of a node
% "Parent" is the name of the node's (single) parent node.
%          For the root node, the parent node name is [].
%
% Below an example. Client code would use_module/1 this module and
% defined its own edge/2 predicate, then call build_term/1.
% ===

/*
edge(a,[]).
edge(b,a).
edge(c,a).
edge(d,b).
edge(e,b).
edge(f,c).
edge(g,c).
edge(h,d).
edge(i,d).
edge(j,e).
edge(k,e).
edge(l,f).
edge(m,f).
edge(n,g).
edge(o,g).
*/

% ===
% Given an edge/2 predicate in the Prolog database describing a tree,
% generate a term describing the same tree, structured as follows:
%
% As usual for Prolog, it is difficult to know whether one should "fail"
% or "throw". The former may be "logical" but gives no hint about what went
% wrong (TODO: add a tracer). The latter carries a good error message (unless
% one of those sadsack ISO exceptions). Is there a reason to "succeed
% with an error message?" (which corresponds to other language's returning
% with a "special value indicating failure", not necessarily a good practice)
% I don't think so!
%
% In fact, section of Prolog execution seem to be subject
% to "fail-ness" (and thus backtracking): those where search is involved and
% others subject to "throw-ness" (and thus trying again or fixing things): those
% where computation I/O etc is involved.
%
% In throw-ness sections, instead of just failing in mystery, be rude and throw!
% ===

build_term([RootName,NodeNames]) :-
   correct_name_check,
   unique_parent_check,
   unique_root_check(RootName),
   node_names(NodeNames).

/*

   node_container_from_names(Names,node_container,Container1,RootName),
   uptick_dict_with_root_entry(RootName,Container1,Container2),
   patch_depth_entries(Container2), 
   prettyprint_dict(Container2).
*/

% ---
% Collect all node names from the Prolog database.
% Throw domain error if there are no nodes!
% ---

node_names(NodeNames) :-
   setof(N,P^edge(N,P),NodeNames)     % collect all node names; fails if there are none
   -> true
   ; domain_error("nonempty set of node names",NodeNames).

% ---
% Check that every node has exactly one parent.
% This can be elegantly done with library(aggregate).
% If the outer bagof/3 succeeds, there are nodes with a parent count
% different from 1, which is bad, and so we throw domain error with info.
% Note that this predicate takes no parameters as it directly accesses the
% database.
% ---

unique_parent_check :-
   bagof(N-Count,
         (aggregate(count,X^edge(N,X),Count),Count\==1),
         Bag)
   -> domain_error("all nodes have exactly one parent",Bag)
   ; true.

% ---
% Checking for a unique root should be easy. bagof(R,edge(R,[]),[_])
% suffices! We make it a bit more complex to give the user a good message.
% ---

unique_root_check(Root) :-
   findall(Root,edge(Root,[]),Roots),
   ((Roots=[Root])
   -> true
   ; domain_error("exactly one root node exists",Roots)).

% ---
% Check that naming is correct in that only 'atom' are used as names
% (except for the parent of the root)
% ---

correct_name_check :-
   findall( 
      edge(N,P), 
      (edge(N,P),(\+atom(N);\+(P==[];atom(P)))),
      BadEdges
   ),
   ((BadEdges==[])
   -> true
   ; domain_error("edges/2 has atom on first and second position",BadEdges)).



/*

% ===
% Building the node container. "RootName" is unified with the name of the
% unique node which has no parent.
% ===

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






% ===
% Special key for Container
% ===

root_entry_key('_root_').

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
% Helpers
% ---

root_name_and_node(Container,RootName,RootNode) :-
   root_entry_key(REK),
   get_dict(REK,Container,RootName),       % fails or RootName is the name of the Root Node
   get_dict(RootName,Container,RootNode).  % get the RootNode out of the Container, too

*/

