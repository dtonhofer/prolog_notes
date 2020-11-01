:- module(heavycarbon_create_random_term,
   [
   node_at_random/3  % Type, Node, Length
   ]).

:- use_module(library('heavycarbon/utils/random_atom.pl')).
:- use_module(library('heavycarbon/utils/randomly_select.pl')).
:- use_module(library('heavycarbon/utils/list_of_numbered_pairs.pl')).
:- include(library('heavycarbon/support/meta_helpers_nonmodular')).

random_size(X,Options) :-
   % TODO: randomly_select should filter!
   randomly_select(
      _{  0  : xx,
          1  : xxx,
          2  : xxxxxxx,
          3  : xxxxxxxxxxxx,
          4  : xxxxxxx,
          5  : xxxx,
          6  : xxx,
          7  : xx,
          8  : xx,
          9  : xx,
          10 : x },XX),
   ((option_nonempty(Options),XX==0) -> random_size(X,Options) ; X=XX).

mc(Type,List) :- 
   memberchk(Type,List).

option_nonempty(Options) :-
  nonvar(Options),
  memberchk(nonempty,Options).

% ---
% Create a "Node" of the given "Type".
% The "Length" parameter relates to a a Type-dependent _length_.
% - Number of (unbound) elements in a "list" ;
% - Number of arguments in a "compound" ;
% - Number of entries in a "dict" ;
% - Number of characters in an "atom" or "string" ;
% - Unused in case a "hole" is requested.
% The "Length" parameter may be:
% - Unbound. Then the _length_ is chosen at random and "Length" is bound to the _length_.
% - Bound to an integer >= 0. Then that value is used as _length_
% Options is a list carrying some set of options (atoms or compound terms).
% Currently recognized options: 'nonempty' to force _length_ to be nonzero.
% --- 

node_at_random(Type,Node,Length,Options) :-
   must_be(atom,Type),
   must_be(var,Node),
   unless(var(Length),must_be(nonneg,Length)),
   unless(var(Options),must_be(list(atom),Options)),
   % switch throws if no case met, so tests the values of Type
   switch(
      mc(Type,[open_list])        , gen_random_open_list(N0,Length,Options),
      mc(Type,[list,closed_list]) , gen_random_closed_list(N0,Length,Options),
      mc(Type,[hole,var])         , (Length=0,gen_hole(N0)),
      mc(Type,[atom])             , gen_atom(N0,Length,Options),
      mc(Type,[string])           , gen_string(N0,Length,Options),
      mc(Type,[compound])         , gen_compound(N0,Length,Options),
      mc(Type,[dict])             , gen_dict(N0,Length,Options)
   ),
   uuid(UUID,[version(4)]),
   put_dict(_{uuid:UUID},N0,Node).

% ---

gen_content_dict(Tag,Dict,Length,Options) :-
   var(Length),
   !,
   random_size(Length,Options),
   gen_content_dict(Tag,Dict,Length,Options). % call yourself with Length nonvar

gen_content_dict(Tag,Dict,Length,Options) :-
   nonvar(Length),
   !,
   if_then(option_nonempty(Options),must_be(positive_integer,Length)),
   list_of_numbered_pairs(Pairs,Length),
   dict_pairs(Dict,Tag,Pairs). % builtin
 
% ---

gen_random_closed_list(N,Length) :- 
   gen_content_dict(x,Content,Length),
   N=node{type:closed_list,length:Length,content:Content}.

gen_random_open_list(N,Length) :- 
   gen_content_dict(x,Content,Length),
   gen_hole(Fin),   
   N=node{type:open_list,length:Length,fin:Fin,content:Content}.


% ---

gen_hole(node{type:hole}).

% ---

gen_atom(node{type:atom,value:A},Length) :-
   random_atom(A,Length). % does not generate the empty atom

% ---

gen_compound(N,Length) :-
   gen_content_dict(x,Args,Length),
   random_atom_1(Name), % does not generate the empty atom
   N=node{type:compound,length:Length,name:Name,args:Args}.

% ---

gen_dict(Node,Length,Options) :-
   gen_content_dict(k,Keys,Length,Options),    % If Length is unbound, it will be bound
   gen_content_dict(v,Values,Length,Options),  % ...and propgated to this call, ensuring identical length
   random_atom_1(Tag),                         % does not generate the empty atom

   N=node{type:dict,length:Length,tag:Tag,keys:Keys,values:Values}.
   
