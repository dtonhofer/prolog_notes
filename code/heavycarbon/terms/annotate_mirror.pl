:- module(heavycarbon_terms_annotate_mirror,
          [
             annotate/2   % annotate(MirrorIn,MirrorOut) 
          ]).

:- use_module(library('heavycarbon/terms/clashfree_id_selection.pl')).
:- load_files(library('heavycarbon/utils/meta_helpers_nonmodular.pl')).

% For efficiency, try to avoid rebuilding the mirrored structure. 
% This can be easily done with generational lists, i.e. open list to
% which one adds a "new generation" as needed. Getting the last
% element of an open list is cheaper than having to create a lot of
% stuff on the heap.
% In that case "annotate" just takes the "Mirror" which can then be
% updated "in place" if it has been appropriately constructed.

% This procedure has to go to the leaves first, then come up.
% It is very "right-recursive"

annotate(hole(X,Id,Depths,Tip)) :-

annotate(atomic(A,Id,Depth,Tip)) :-
   with_output_to(string(Txt),format("~q",[A])),
   string_length(Txt,Len),
   

annotate(compound(Id,Depth,Name,ChildIds,Tip)) :-

annotate(dict(Id,Depth,DictTag,KeyIds,ValueIds,Tip)) :-
   
   
tippy_append(Tip,Payload) :-

append_to_open_list([_|More],Payload,NewFin) :- !,append_to_open_list(More,Payload,NewFin).
append_to_open_list(Fin,Payload,NewFin)      :- var(Fin),!,Fin=[Paylod|NewFin].
append_to_open_list(Fin,Payload,NewFin)      :- throwme(append_to_open_list,not_an_open_list).




















% ---
% Entry
% ---

make_analyzable(NodeIn,NodeOut,LookupNodes) :-
   copy_term(NodeIn,NodeOut),               % Create a deep copy of the structure reachable from NodeIn with "fresh holes"; unify it with NodeCopied.    
   split_edges([NodeOut]).                  % Split all edges of NodeOut; there is no output as this is done "in place"
                                            % Note the change in perspective: Formerly, there was "the graph reachable from NodeOut", now we take the Nodes
                                            % one-by-one as they appear in the list at arg1 position, only considering the children; starting at the entry node NodeOut
                                            % As we side-effect the term store, there seems to mysteriously be no output.

% ---
% Split all discoverable edges
% ---

% termination condition: no further nodes to process

split_edges([]).            

% a hole or atomic? Nothing to do! Proceed to next node in the list.
  
split_edges([N|Ns]) :- 
   (var(N);atomic(N)),!,
   split_edges(Ns).

% compound but not a dict? (dict is a problem, and so is is_dict/1; to be examined!)
% split edges to chil nodes, if this hasn't been done already

split_edges([N|Ns]) :- 
   compound(N),\+is_dict(N),!,
   split_edges_to_children(N,Ns,ExtendedNs), % doing this immediately covers the case where one of the children of N is N itself - this WILL be detected!
   split_edges(ExtendedNs). 

% ---
% Split all edges from compound node N to its child nodes
% If it turns out that the egdes to the child nodes have already been split, nothing happens at all!
% ---

split_edges_to_children(N,Ns,ExtendedNs) :- 
   compound_name_arguments(N,_Name,Args),
   split_edges_to_children_2(Args,'>>',SplittedArgs,MoreNs,Case), % the '>' is just there for clarity to separate Input and Outpt - an experiment!!
   Case == accept 
   -> inject_splitted_args(N,SplittedArgs),
      append(MoreNs,Ns,ExtendedNs).
   ;
      ExtendedNs=Ns.

% ---
% Helper
% ---

% Compound of arity 0; no args, nothing to do

split_edges_to_children_2([],'>>',[],[],accept) :- !.

% Compound of arity >= 1.
% These children may already have been split off, but we need to find out!
% If we encounter a SplitterNode with name '⇊', we know the nodes have already been split off!
% If we encounter a compound node with a name different from '⇊', we know the nodes have not yet been split off
% Nodes that are atomic or holes are left as is, we don't bother to insert splitter nodes.
% We are using the atom '⇊' as the name of the node found on a split edge to
% avoid clashes. Note that this node only has one child: the original argument node! 

split_edges_to_children_2([A|As],Ns,'>>',[A|SplittedArgs],ExtendedNs,YN) :-
   (var(A);atomic(A)),!,
   split_edges_to_children_2(As,Ns,'>>',SplittedArgs,ExtendedNs,YN).

split_edges_to_children_2([A|As],Ns,'>>',[],Ns,reject) :-
   compound(A),\+is_dict(A),compound_name_arity(A,'⇊',_),!. % terminate recursion early

split_edges_to_children_2([A|As],Ns,'>>',['⇊'(A)|SplittedArgs],[A|ExtendedNs],YN) :- % A appears in ExtendedNs will be examined later!
   compound(A),\+is_dict(A),!,
   assertion(\+compound_name_arity(A,'⇊',_)),
   split_edges_to_children_2(As,Ns,'>>',SplittedArgs,ExtendedNs,YN).
   



   
 
   split_edges_to_children_2(As,Ns,'>>',ExtendedNs).


'⇊'(_)|_],Ns,'>>',Ns) :- !,
   assertion(maplist([A]>>(atomic(A);compound_name_arity(A,'⇊',_)),Args)).

% Default case: Compound of arity >= 1, and first node is not a node that has been
% inserted into a split edge. So none of the edges has been split, and we need to split them.

split_edges_to_children_2(Args,Ns,'>>',ExtendedNs) :-
   assertion(Args = [A|As]), 
   assertion(maplist([A]>>(atomic(A);\+compound_name_arity(A,'⇊',_)),Args)),
   maplist([A,'⇊'(A)]>>true,Args,SplitterNodes),
   append(SplitterNodes,Ns,ExtendedNs).



   fixup_compound(MarkedArgs,Node,NewlyMarkedSubnodes).

fixup_compound([],_,[]).
fixup_compound([MA|MAs],Node,[MA|MAs]) :- setargs_to_marked_args(Node,[MA|MAs],1).
   
setargs_to_marked_args(Node,[MA|MAs],I) :-
   setarg(MA,INode,I),
   succ(I,Ip),
   setargs_to_marked_args(Node,MAs,Ip).

setargs_to_marked_args(Node,[],_).



 
   Args == [] -> nothing to do
   Args = [A|Args]
      A == marker(X) -> one edge has been marked, then all edges have been marked, so we are done
      -> first edge has not been marked -> none are marked, marke them all!
      mark_all_edges(Args,Marks),
      % decouple examination order
      


mark_all_edges([],[]).
mark_all_edges([A|Args],[mm(A)|Marks]) :- mark_all_edges(Args,Marks).
   
maplist([A,mm(A)]>>true,Args,Marks).









% ===
% Vocabulary!
%
% Term         - Either a variable appearing in source code or the structure designated/named by that variable. 
%                Prolog should have separated these two things. More generally, what may inside parentheses in 
%                a Prolog program            
% Variable     - Source code level: A name that designates something in the Term Store examples A, _, _A. Local to the clause.
%                What is between the parenthesis of var(X) with the compiler not complaining
%                Runtime level: A reference into the term store which may change as computation progresses.
%                Also: The variable "holds a term"
%                Various variables may designate various places of the same term (but there is no way of find out about that!
% Hole         - A designated entry in the Term store which as yet holds no Term Node. The hole can be 
%                assigned a Term Node as computation progresses (it can be bound or instantiated) 
%                The hole cannot be de-assigned except on backtracking (during failure or exception)
%                Once a hole has been assigned 
%                Varoiables can designate the "same hole" (in effect, there is a "equality constraint" implemented via sharing)
%                Holes are graph leaves because there is no outgoing edge.
% Freshvar     - A variable that designates a hole. When a variable is first used in a clause, it is "fresh" (and may sta so)
%                var(X) succeeds if X is a freshvar.
%                fresh(X) should be the actual name for var(X)
% Term Store   - The storage area for Term Graphs
% Term Node    - An element of a Term Graph
%                May be atomic (indeed, may be an atom), in which case it can have content of several types
%                May be compound (in which case it may either be a name which is an atom (often called the functor) + fixed number of arguments 
%                      ... or on opaque data structure with internal structure, like a dic
%                In some cases, the term node may be OPAQUE (it designates an immutable THING via a BLOB, which in turn may
%                designate some nonlogical thing outside ... like a stream)
% Term Graph   - A Structure in the Term Store. Term Nodes linked by directed edges. May be 
%                atomic, in particular may be an atom
%                compound if one looks at the entry node reference 
%                It's a directed, cyclic graph, with labels on the nodes and no labels on the edges.
%                Note that term graphs are never out-of-bound to prolog programs - they can be fully examined
% Downstream term graph: DTG The subgraph reachable from a given Term Node
%                Reachable term graphs with the same structure as indistinguishable. They mabe physically 
%                the same ... or not! It's not important (excpet for performance considerations, which are considerable)
% Root node    - The node of a Term graph designated by a variable this may or may not be the root node of a graph
% Entry node   - Same               
% Term Graph Source Node - The "topmost node" of a graph. There may be several
% to refine: to fill a hole (to constrain would also wok, but I want to keep this word for CLP); to sharpen maybe?
% to upversion: to generate a new accumulator value from an existing accumulator value or upgrade
% ===

% Future: Additionally compress the tree, finding equal subgraphs (there are bound to be some!)
% This can be easily done by hashing from the leaves upwards

 
% ===
% Dict upversioning
% ===

% Find the entry in the Dict which mirrors the hole designated by variable Var.
% i.e. find the entry hole(X) suc that X and Var are *equivalent* (using ==).
% Id is then set to the key of this entry.
% The predicate fails if there is no such entry.
% Finding an entry like this may be slower than using a dedicated "variable 
% lookup" structure. Maybe. But having everything in the same dict is fine too.
% 234567890123456789012345678901234567890123456789012345678901234567890123456789


% Sort entried by decreasing depth

depth_sorted(Dict,List) :-
   collect(Dict,List).



/*
% ---
% Start texification
% ---

% Begin with the entries of highest "depth"
% Assuming that all the entries of higher depth have already been handled, then:
% Given the Id of a node, augment it with data. This transform "DictIn" to "DictOut".

textify(Id,DictIn,DictOut) :-
   get_dict_ex(Id,DictIn,mirror(Id,Depth,What,AttrDict)), % exception if not found
   textify_casey(What,Id,DictIn,AttrDict).  % ETC

% compound terms will have the node's name and underneath subterms

textify_casey(compound(Name,Args),Depth,Id,DictIn,AttrDict,AttrDictOut) :-
   length(Args,Arity),
   with_output_to(string(Text),format("~q/~d",[Datum,Arity])),
   string_length(Text,Length),
   put_dict([text-Text,width-Length,heigh ... ],AttrDictIn,AttrDictOut).
   

% variables will be printed with a long line going to the right but otherwise
% will just be 1 character

textify_casey(var(X),Depth,Id,DictIn,AttrDictIn,AttrDictOut) :-
   put_dict([text-"⨂",width-1,height-1],AttrDictIn,AttrDictOut).
   
% atoms will be printed as the are

textify_casey(atomic(Datum),Depth,Id,DictIn,AttrDictIn,AttrDictOut) :-
   with_output_to(string(Text),format("~q",[Datum])),
   string_length(Text,Length),
   put_dict([text-Text,width-Length,height-1],AttrDictIn,AttrDictOut).
 

% ---
% Textification structure
% ---

% - We have a dict with integer keys starting from 0 for the lines (rows).
% - Each line, i.e. each value of the dict, is a string.
% - The dict is called "screen{}"
% - We need just a "poke" function which updates the screen at a certain
%   (row,col) with a certain "string".

poke(ScreenDictIn,Row,Col,Val,ScreenDictOut) :-
   atom_string(Val,Str),            % make sure this is a string
   assertion(integer(Col),Col>=0),
   assertion(integer(Row),Row>=0),
   string_length(Str,StrLen),
   ((StrLen>0) -> 
    poke_nonempty(ScreenDictIn,Row,Col,Str,ScreenDictOut) ; true).

poke_nonempty(ScreenDictIn,Row,Col,Str,ScreenDictOut) :-
   (get_dict(Row,ScreenDictIn,Line) 
    -> poke_line(Line,Col,Str,NewLine)
    ;  poke_line(""  ,Col,Str,NewLine)),
   put_dict(Row,ScreenDictIn,NewLine,ScreeDictOut).


:- begin_tests(term_display).

   start_dict(_{}).

   test(var)      :- 
      start_dict(Dc),mirror_term_tree(_SomeVar,0,Dc,DcOut,Id), format("~q ~q\n",[DcOut,Id]).

   test(atomic)   :- 
      start_dict(Dc),mirror_term_tree("hello",0,Dc,DcOut,Id), format("~q ~q\n",[DcOut,Id]).

   test(compound) :- 
      start_dict(Dc),mirror_term_tree(f(x,g(y,h(k)),Z),0,Dc,DcOut,Id), format("~q ~q\n",[DcOut,Id]).
 
:- end_tests(term_display).

rt(term_display) :- run_tests(term_display).
*/
