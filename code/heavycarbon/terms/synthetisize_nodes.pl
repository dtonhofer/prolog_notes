% TODO: Handle dict specially (even though for now it's a compound term)
% Replace vars by uniquely identifiable blobs to avoid accidents
% Reverse synth_term to recreate the Term
% Detect cycles
% Create random networks. By creating synths of course. The network can have arbitrary cylces
% How can we be sure this is correct?


:- begin_tests(top).

test(1,true(SynthTerm==R)) :- synth_term(X,SynthTerm), R = synth{node:X, type:var}.
test(2,true(SynthTerm==R)) :- synth_term(foo,SynthTerm), R = synth{node:foo, type:atomic}.
test(3,true(SynthTerm==R)) :- synth_term(10001,SynthTerm), R = synth{node:10001, type:atomic}.
test(4,true(SynthTerm==R)) :- synth_term(f(a),SynthTerm), R = synth{1:synth{node:a, type:atomic}, arity:1, name:f, node:f(a), type:compound}.

:- end_tests(top).

% ===
% Testing
% ===

:- begin_tests(lookup).

 % Term1=f(a,b,c),empty_synthmap(SynthMap0),lookup_or_create(Term1,SynthNode1,SynthMap0,SynthMap1,Status1).
   
test(1) :-
   Term1=f(a,b,c),
   empty_synthmap(SynthMap0),
   
   % lookup_or_create(@Node,-SynthNode,+SynthMapIn,-SynthMapOut,-Status),
   
   lookup_or_create(Term1,SynthNode1,SynthMap0,SynthMap1,Status1),   
   assertion(Status1==created),
   
   lookup(Term1,_,SynthMap1,SynthMap2,Status2),   
   assertion(Status2==found),
   
   Term2=f(a,b,c),
   
   lookup_or_create(Term2,SynthNode2,SynthMap2,SynthMap3,Status3),   
   assertion(Status3==created),
   
   format("Term1: ~q, SynthTerm1: ~q\n",[Term1,SynthNode1]),
   format("Term2: ~q, SynthTerm2: ~q\n",[Term2,SynthNode2]),
   format("SynthMap: ~q\n",[SynthMap3]).
   
:- end_tests(lookup).

zsucc(X,Y) :-
  (nonvar(X) -> (integer(X) -> true ; type_error(integer,X)) ; true),
  (nonvar(Y) -> (integer(Y) -> true ; type_error(integer,Y)) ; true),
  ((var(X),var(Y)) -> instantiation_error("Need at least one value instantiated") ; true),
  (nonvar(X)
     -> Y is X+1
     ;  X is Y-1).

% =============================================================================
% Entry: Synthetisize a "term graph" with root node "Node" into a new graph 
% where SWI-Prolog dicts take the role of nodes in the original "term graph".
% The "SynthNode" is the root node of this mirrored "term graph".
%
% synth_term(+Term,-SynthTerm)
% =============================================================================

synth_term(Node,SynthNode) :-
   (compound(Node);is_dict(Node)) ->
   (
      empty_synthmap(SynthMap0),
      lookup_or_create(Node,SynthNode,SynthMap0,SynthMap1,Status),  % this synthesizes not the whole term, just the entry node (an possibly a second node in case of dict)     
      assertion(Status==created),                                   % must have been created
      synth_graph([SynthNode|Fin],Fin,SynthMap1,_SynthMapOut)       % kick off recursive traversal of graph rooted in SynthNode by setting up an open list difference list "[SynthNode|Fin]-Fin"
   )   
   ; 
   create_synth_node(Node,SynthNode). % no need to set up an use a SynthMap, create directly!
      
% =============================================================================
% Synthesize the original graph. A FIFO of already synthesized nodes whose
% arguments (child nodes) must be examined for synthetization is passed as an
% open list difference list in the first two parameters. Newly created
% synthesized nodes are appended at Fin. This difference list initially
% contains only the original term graph's synthesized root node.
%
% In accumulator fashion:
% 
% SynthMap1 is the initial lookup map of encountered nodes.
% SynthMap2 is the final lookup map of encountered nodes. 
%
% synth_graph(+Tip,+Fin,+SynthMap1,-SynthMap2)
% =============================================================================

% A helper to perform assertions

synth_graph_assertions(SynthNode) :-
   % Coming from the (FIFO) buffer, this SynthNode is:
   % 0) A synth node
   % 1) Referencing an original compound node or a dict (the node it synthesizes)
   % 2) All of its arguments (with index 1 ... SynthNode.Arity) denote original, 
   %    non-synth nodes (although there might already be a synth node for them)
   %    otherwise we wouldn't be examining it.
   assertion(is_synth_node(SynthNode)),
   assertion(memberchk(SynthNode.type,[compound,dict])),  
   index_range(SynthNode.type,SynthNode.arity,FromIndex,MaxIndex),
   synth_graph_assertions_each_child(SynthNode,FromIndex,MaxIndex).
   
synth_graph_assertions_each_child(_,Index,MaxIndex) :-
   Index > MaxIndex,!.
   
synth_graph_assertions_each_child(SynthNode,Index,MaxIndex) :-
   Index =< MaxIndex,
   assertion(\+ is_synth_node(SynthNode.Index)),
   zsucc(Index,NextIndex),
   synth_graph_assertions_each_child(SynthNode,NextIndex,MaxIndex).

% Tip==Fin means the buffer is empty and there is nothing left to do

synth_graph(Tip,Fin,SynthMap,SynthMap) :- 
   Tip==Fin,!.
   
% First Tip\==Fin then synthesize the first node in the buffer 
% For compound terms:
% - handle all the compound term arguments (graph children), in the map from index 1 to index arity
% For dicts:
% - handle all the dict keys, in the map from index 1 to arity
% - handle all the dict values, in the map from index -arity to -1 (where index -x corresponds to key x)
% - handle the tag, in the map at index 0 (the tag should not but actually can be, a compound term!)
% At the very end of synth_graph, perform recursive call to 
% continue with the rest of the FIFO buffer, which may or may not have grown at its "Fin", giving a new "Fin".
   
synth_graph([SynthNode|MoreSynthNodes],Fin,SynthMapIn,SynthMapOut) :-
   synth_graph_assertions(SynthNode),
   index_range(SynthNode.type,SynthNode.arity,FromIndex,MaxIndex), % indexes are inclusive
   synth_node_children(SynthNode,FromIndex,MaxIndex,SynthMapIn,SynthMap2,Fin,Fin2),         
   synth_graph(MoreSynthNodes,Fin2,SynthMap2,SynthMapOut). 

index_range(compound, Arity,1     ,Arity). % compound child index ranges 1..arity
index_range(dict    , Arity,NegArity,Arity) :- NegArity is -Arity. % dict child index ranges -arity..-1 (values), 0 (tag), +1..+arity (keys)
   
% =============================================================================
% Verify that SynthNode is indeed a SynthNode.
% =============================================================================

is_synth_node(SynthNode) :- 
   is_dict(SynthNode,Tag),
   Tag==synth. % do not unify directly above but compare! The could be an unbound variable!
   
% =============================================================================
% Work off the children of a synth node representing a compound or dict
% =============================================================================

synth_node_children(_,Index,MaxIndex,SynthMap,SynthMap,Fin,Fin) :- 
   Index>MaxIndex,!.

synth_node_children(SynthNode,Index,MaxIndex,SynthMapIn,SynthMapOut,FinIn,FinOut) :-
   Index =< MaxIndex,
   Child = SynthNode.Index,  % Named directly after the numeric index
   lookup_or_create(Child,SynthNodeOfChild,SynthMapIn,SynthMap2,Status),   
   b_set_dict(Index,SynthNode,SynthNodeOfChild), % "Index" is the Key in "SynthNode", the value of which we set to "SynthNodeOfChild"
   (
      (Status == created,memberchk(SynthNodeOfChild.type,[compound,dict]))
      ->
      FinIn=[SynthNodeOfChild|Fin2] % append newly created compound/dict node for later processing of its children
      ;
      Fin2=FinIn
   ),
   zsucc(Index,NextIndex),
   synth_node_children(SynthNode,NextIndex,MaxIndex,SynthMap2,SynthMapOut,Fin2,FinOut).

% =============================================================================
% Look up the synth node corresponding to the entry node "Node"  
% in the synth map. If it exists, it is unified with "SynthNode" and
% "Status" is unified with "found". If it doesn't exist, 
% "SynthMapOut" is "SynthMapIn" with an added synth node "SynthNode"
% corresponding to "Node", and "Status" is unified with "created".
%
% lookup_or_create(@Node,-SynthNode,+SynthMapIn,-SynthMapOut,-Status),
% =============================================================================

% it exists already!

lookup_or_create(Node,SynthNode,SynthMap,SynthMap,found) :-
   lookup_in_synthmap(Node,SynthMap,SynthNode),!.
   
% it doesn't exist yet!

lookup_or_create(Node,SynthNode,SynthMapIn,SynthMapOut,created) :-
   create_synth_node(Node,SynthNode),
   extend_synthmap(Node,SynthNode,SynthMapIn,SynthMapOut), % It's in the SynthMap now
   assertion(lookup_or_create(Node,_,SynthMapOut,_,found)).
   
% =============================================================================
% Create a new synth node for Node holding all data about Node (including
% a reference to Node itself)
% =============================================================================

create_synth_node(Node,SynthNode) :-
   compound_or_dict_term_data(Node,Data), % if it's not a compound term, then Data is the empty list
   type_code(Node,TypeCode),
   dict_create(SynthNode,synth,[node-Node,type-TypeCode|Data]). % this creates the SynthNode, an SWI-Prolog dict   
   
% ---
% Helper for create_synth_node/2
% --

type_code(Node,dict)     :- is_dict(Node,_),!. % must come before compound before a dict currently _is_ compound
type_code(Node,compound) :- compound(Node),!.
type_code(Node,atomic)   :- atomic(Node),!.
type_code(Node,var)      :- var(Node),!.

% =============================================================================
% Give me an empty synthmap
% =============================================================================

empty_synthmap(synthmap{}).

% =============================================================================
% Create a key for the first-level lookup in a synth map.
%
% first_level_key(+Node,-FirstLevelKey)
% =============================================================================

first_level_key(Node,Key) :-  % must come before compound before a dict currently _is_ compound
   is_dict(Node),!, 
   dict_pairs(Node,_,Pairs),
   length(Pairs,Arity),
   atomic_list_concat([dict,'_',Arity],Key).
   
first_level_key(Node,Key) :-
   compound(Node),!,   
   compound_name_arity(Node,_,Arity),
   atomic_list_concat([compound,'_',Arity],Key).

first_level_key(Node,Key) :-
   atomic(Node),!,
   (float(Node)   -> Key = float ;
    integer(Node) -> Key = integer ;
    number(Node)  -> Key = number ;
    string(Node)  -> Key = string ;
    atom(Node)    -> (atom_length(Node,L), atomic_list_concat([atom,L],Key)) ;
    blob(Node,reserved_symbol) -> Key = reserved ;
    Key = atomic).

first_level_key(Node,var) :-
   var(Node).
   
% =============================================================================
% Succeeds with the looked-up "SynthNode" unified if there is an entry
% for "Node" in "SynthMap", otherwise fails.
%
% lookup_in_synthmap(+Node,+SynthMap,-SynthNode)
% =============================================================================

lookup_in_synthmap(Node,SynthMap,SynthNode) :- 
   first_level_key(Node,FirstLevelKey),
   get_dict(FirstLevelKey,SynthMap,SynthSubMap), % fails if not exists
   !, % first key exists in SynthMap
   lookup_in_synthsubmap(Node,SynthSubMap,SynthNode).
   
% =============================================================================
% Succeeds with the appropriate "SynthNode" unified if there is an entry
% for "Node" in "SynthSubMap" (which is a list of Node-SynthNode pairs), 
% otherwise fails.
%
% lookup_in_synthsubmap(+Node,+SynthSubMap,-SynthNode)
% =============================================================================

lookup_in_synthsubmap(Node,[Key-SynthNode|_],SynthNode) :- 
   same_term(Node,Key),
   !.
   
lookup_in_synthsubmap(Node,[_|More],SynthNode) :- 
   lookup_in_synthsubmap(Node,More,SynthNode).

% =============================================================================
% Extend "SynthMap" with a new entry for SynthNode.
% The entry is added to a list of Node-SynthNode pairs,
% which is added to the SynthMap if needed, where SynthMap is
% an SWI-Prolog dict mapping first-level-key to the list of
% Node-SynthNode pairs. The new entry is supposed to not exist yet.
% =============================================================================

extend_synthmap(Node,SynthNode,SynthMapIn,SynthMapOut) :-   
   assertion(\+ lookup_in_synthmap(Node,SynthMapIn,_)),
   first_level_key(Node,FirstLevelKey),
   (get_dict(FirstLevelKey,SynthMapIn,SynthSubMap) -> true ; SynthSubMap = []), 
   put_dict(FirstLevelKey,SynthMapIn,[Node-SynthNode|SynthSubMap],SynthMapOut).
   
% =============================================================================
% If "Node" is a compound term,. create a list of key-value pairs: one for the
% arity (arity-Arity), one for the functor name (name-Name) and one for each
% argument (position-Arg). The position is the traditional 1-based indexing.
% If "Node" is not a compound term, the list is empty.
%
% compound_term_data(+Node,-Data)
% =============================================================================

compound_or_dict_term_data(Node,[0-Tag,arity-Arity|SynthNodePairs]) :-   % must come before compound before a dict currently _is_ compound
   is_dict(Node),
   !,
   dict_pairs(Node,Tag,Pairs),
   length(Pairs,Arity),
   pairify_dict(Pairs,1,SynthNodePairs).
    
compound_or_dict_term_data(Node,[name-Name,arity-Arity|SynthNodePairs]) :-
   compound(Node),
   !,
   compound_name_arguments(Node,Name,Args),
   length(Args,Arity),
   pairify_compound(Args,1,SynthNodePairs).

compound_or_dict_term_data(_,[]). % default
   
% =============================================================================
% Transform a list of arguments (from a compound term) into a list of pairs
% "Index-Argument" where Index is the running index.
% =============================================================================

pairify_compound([Arg|Args],Index,[Index-Arg|ArgPairs]) :- 
   !,
   zsucc(Index,NextIndex), 
   pairify_compound(Args,NextIndex,ArgPairs).
   
pairify_compound([],_,[]).

% =============================================================================
% Transform a list of arguments (key-value pairs from a dict) into a list of 
% pairs "Index-Key", "(-Index)-Value" where Index is the running (positive)
% index.
% =============================================================================

pairify_dict([Key-Value|Args],Index,[Index-Key,NegIndex-Value|ArgPairs]) :- 
   !,
   NegIndex is -Index,
   zsucc(Index,NextIndex), 
   pairify_dict(Args,NextIndex,ArgPairs).
   
pairify_dict([],_,[]).
