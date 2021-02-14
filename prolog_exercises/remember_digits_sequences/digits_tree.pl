:- module(digits_tree,
          [
           in_tree/2      % in_tree(+Digits,Dict) 
          ,add_to_tree/4  % add_to_tree(+Digits:List,+RootIn:Dict,-RootOut:Dict,Changes:Boolean) 
          ]).

:- use_module(library(debug)).
  
% =============================================================================
% We have a set of of lists of N digits, N constant.
%
% Whenever we are give a new list, we want to find out whether a list has
% already been encountered earlier.
%
% If it has not been encountered yet, it shall be added to a set of lists
% already seen, and considered in the next request.
% 
% The data structure to take up the lists shall be a tree, with each node
% (including the root node) a dict.
%
% It is best to think of edges as decorated with the digits. An
% edge connection a node at depth K with a node at depth K+1 is is
% labeled with the K-th digit from the list (K being 0-based).
% Thus what counts at the lowermost entries of the tree are the edges,
% not the leaf nodes. In fact, the leaf nodes can be arbitrary things as
% all the lists have constant length and there won't be any growing at the
% leaf nodes. We use [] as the leaf nodes.
%
% The empty tree is an empty root node, i.e. an empty dict.
%
% An inner node a depth K>0 (depth 0 the root) will be found by traversing the
% list of digits, indexed by 0...K-1 and using each digit as key to find the
% edge to a child node.
%
% The tag of all dicts is arbitrarily set as 't'.
%
% Example:
%
/*
  ?- 
  add_to_tree([1,2,3,4],t{},Tree1,Change1),
  add_to_tree([1,2,3,5],Tree1,Tree2,Change2),
  in_tree([1,2,3,5],Tree2).

  Tree1 = t{1:t{2:t{3:t{4:[]}}}},
  Change1 = Change2, Change2 = true,
  Tree2 = t{1:t{2:t{3:t{4:[],5:[]}}}}.
*/
% =============================================================================

% ==========
% in_tree(+DigitSequence:List,+Node:Dict) 
% Succeeds or fails depending on whether sequence Digits is in the dict-based tree
% rooted at Node or not.
% ==========

% the empty sequence is really the "termination case"

in_tree([],[]). 

% the nonempty sequence 

in_tree([X|Xs],Node) :-
   get_dict(X,Node,SubNode), % if it fails, it's not in the tree
   in_tree(Xs,SubNode).
   
% ==========
% add_to_tree(+Digits:List,+RootIn:Dict,-RootOut:Dict,Changes:Boolean) 
%
% Always succeeds. Adds the sequence Digits to the dict-based tree rooted at NodeIn or not. 
% On succeess, Changes == true  -> NodeOut is a new tree built from NodeIn, containing the Digits sequence
%              Changes == false -> NodeOut = NodeIn
% ==========
   
add_to_tree([],[],[],false).

add_to_tree([X|Xs],NodeIn,NodeOut,Changes) :-
   get_dict(X,NodeIn,SubNode),
   !,
   add_to_tree(Xs,SubNode,SubNodeNew,Changes),
   on_changes(Changes,NodeIn,NodeOut,X,SubNodeNew).

add_to_tree([X|Xs],NodeIn,NodeOut,true) :-
   \+ get_dict(X,NodeIn,_),
   !,
   build_branch(Xs,SubNodeNew),
   put_dict(X,NodeIn,SubNodeNew,NodeOut).
   
% ---
% build_branch(+Digits:List,-NodeOut:Dict)
%
% Helper to build a completely subtree (actually just a chain ~ list) where every node has exactly one child for
% the sequence "Digits".
% ---

build_branch([],[]).

build_branch([X|Xs],NodeOut) :-
   build_branch(Xs,SubNode),
   put_dict(X,t{},SubNode,NodeOut). % Arbitrarily use tag 't'

% ---
% on_changes(+Changes:Boolean,+NodeIn:Dict,-NodeOut:Dict,+Key,+SubDict)
%
% Helper to build NodeOut from NodeIn and add the mapping Key -> SubDict
% if Changes = true
% ---

on_changes(true,NodeIn,NodeOut,Key,SubDict) :-
   put_dict(Key,NodeIn,SubDict,NodeOut).
   
on_changes(false,Node,Node,_,_).
