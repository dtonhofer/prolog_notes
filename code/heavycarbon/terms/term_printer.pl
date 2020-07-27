:- module(heavycarbon_term_graph, [dtg_mirror/3]).

:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).
:- use_module(library('heavycarbon/terms/clashfree_id_selection.pl')).
:- load_files(library('heavycarbon/utils/meta_helpers_nonmodular.pl')).

% 234567890123456789012345678901234567890123456789012345678901234567890123456789
% Deep-copying a term, inserting marker nodes into edges to be able to 
% find cyclic terms
% 234567890123456789012345678901234567890123456789012345678901234567890123456789


% ---
% Entry
% ---

make_analyzable(NodeIn,NodeOut,LookupNodes) :-
   copy_term(NodeIn,NodeOut),               % Create a deep copy of the structure reachable from NodeIn with "fresh holes"; unify it with NodeCopied.    
   split_edges([NodeOut],_{},LookupNodes).  % Split all edges of NodeOut; there is no output as this is done "in place"; however a node lookup structure (dict) is created
                                            % Note the change in perspective: Formerly, there was "the graph reachable from NodeOut", now we take the Nodes
                                            % one-by-one as they appear in the list at arg1 position, only considering the children; starting at the entry node NodeOut

% ---
% Analysis
% ---

split_edges([N|Ns],Lin,Lout) :-
   var(N),!,
   

split_edges(Nodes).
split_edges([N|Ns]) :- atomic(N),!,analyze(Nodes).
split_edges([N|Ns]) :- compound(N),!,analyze_compound(Node,SubNodes),append(SubNodes,Nodes,MoreNodes),analyze(MoreNodes).




analyze_compound(Node,NewlyMarkedSubnodes) :-
   compound_name_arguments(Node,Name,Args),
   analyze_compound_2(Node,Name,Args,'>',MarkedArgs),   
   fixup_compound(MarkedArgs,Node,NewlyMarkedSubnodes).

fixup_compound([],_,[]).
fixup_compound([MA|MAs],Node,[MA|MAs]) :- setargs_to_marked_args(Node,[MA|MAs],1).
   
setargs_to_marked_args(Node,[MA|MAs],I) :-
   setarg(MA,INode,I),
   succ(I,Ip),
   setargs_to_marked_args(Node,MAs,Ip).

setargs_to_marked_args(Node,[],_).

% Compound of arity 0; nothing to do; no subnodes!

analyze_compound_2(_Node,_Name,[],'>',[]) :- !.

% Compound of arity >= 1, but first node has been marked already,
% so ALL of the nodes have been marked - nothing to do.

analyze_compound_2(_Node,_Name,[mm(_)|_],'>',[]) :- !.

% Default: case: Compound of arity >= 1, and first node has **not** been 
% marked already, so NONE of the nodes has been marked - mark them all

analyze_compound_2(_Node,_Name,Args,'>',MarkedArgs) :- maplist([X,mm(X)]>>true,Args,MarkedArgs).



 
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

% 234567890123456789012345678901234567890123456789012345678901234567890123456789


const(max_id,10000000).


dtg_mirror(EntryNode,DictOut,Id) :-
   dtg_mirror(EntryNode,0,mirror{},DictOut,Id).

% ===
% Traverse the DTG, creating a "mirror node" in a lookup dict for each term node
% At each node, extend "DictIn" to "DictOut", carrying the new information.
% Also refine a symbolic "Id" which is the identifier of the mirror node in the
% dict. 
% ===

% Case of: The "entry node" is a "hole" (and thus a leaf)

dtg_mirror(EntryNode,Depth,DictIn,DictOut,Id) :-
   var(EntryNode),!,
   if_then_else(
      (find_hole(DictIn,EntryNode,Id)),
      (DictOut=DictIn),                                           % it exists already!
      (enlarge_dict(hole,EntryNode,Depth,DictIn,DictOut,Id))). % it's new!

% Case of: The "entry node" is atomic (and thus a leaf)
% At this point, we don't care whether there already is an identical entry. Maybe later.
% This means all the mirrored leaves are used by at most 1 mirrored compound.

dtg_mirror(EntryNode,Depth,DictIn,DictOut,Id) :-
   atomic(EntryNode),!,
   enlarge_dict(atomic,EntryNode,Depth,DictIn,DictOut,Id).

% Case of: The "entry node" is compound 

dtg_mirror(EntryNode,Depth,DictIn,DictOut,Id) :-
   compound(EntryNode),!,
   compound_name_arguments(EntryNode,Name,Args),
   % The child nodes may include this very node (a cycle of length 1) 
   % So we have to upgrade the dict twice: once with an incomplete entry
   % because the child nodes are not known yet; once with a complete entry
   % once the child nodes are all known.
   % OTOH, we can't detect the cycle yet :-(
   length(Args,ArgsLength),
   enlarge_dict(compound,Name,ArgsLength,EntryNode,Depth,DictIn,Dict2,Id),
   succ(Depth,MoreDepth),
   dtg_mirror_args(Args,MoreDepth,Dict2,Dict3,ArgIds),
   fix_dict(compound,ArgIds,Id,Dict3,DictOut).

% Mirror the term graphs formed at each argument of a compound term

dtg_mirror_args([Arg|Args],Depth,DictIn,DictOut,[Id|Ids]) :-
   dtg_mirror(Arg,Depth,DictIn,DictMid,Id),
   dtg_mirror_args(Args,Depth,DictMid,DictOut,Ids).

dtg_mirror_args([],_,Dict,Dict,[]).
 
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


% Dict contains
% Id ---> hole(X,Depth)  easily unifiable, var(X) is true
% Id ---> atomic(Id,Depth,Original,_{}) 
% Id ---> compound(Id,Depth,Name,[Args],Original,_{})

find_hole(Dict,Hole,Id) :-
   assertion(var(Hole)),
   % Decorate with "once" to stop at the first (and unique) element.
   % bagof/3 fails if there is no solution, which is what we want!
   bagof(IdFound, once( (get_dict(IdFound,Dict,hole(X,_)),X==Hole) ), [Id]).

enlarge_dict(hole,Hole,Depth,DictIn,DictOut,Id) :-
   assertion(var(Hole)),
   const(max_id,MaxId),
   clashfree_id_selection(DictIn,Id,MaxId),
   put_dict(Id,DictIn,hole(Hole,Depth),DictOut).
 
enlarge_dict(atomic,Atomic,Depth,DictIn,DictOut,Id) :-
   assertion(atomic(Atomic)),
   const(max_id,MaxId),
   clashfree_id_selection(DictIn,Id,MaxId),
   put_dict(Id,DictIn,atomic(Id,Depth,Atomic,_{}),DictOut).
 
enlarge_dict(compound,Name,ArgsLength,Compound,Depth,DictIn,DictOut,Id) :-
   assertion(compound(Compound)),
   const(max_id,MaxId),
   clashfree_id_selection(DictIn,Id,MaxId),
   put_dict(Id,DictIn,compound(Id,Depth,Name,placeholder(ArgsLength),Compound,_{}),DictOut).

fix_dict(compound,ArgIds,Id,DictIn,DictOut) :-
   get_dict(Id,DictIn,
      compound(Id,Depth,Name,placeholder(_ArgsLength),Compound,DataDict),
      DictOut,
      compound(Id,Depth,Name,ArgIds,Compound,DataDict)).

% ===
% Textification
% ===

% Sort entried by decreasing depth

depth_sorted(Dict,List) :-
   collect(Dict,List).

collect(



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
