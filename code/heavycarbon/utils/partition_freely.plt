:- use_module('partition_freely.pl').

% ---
% Testing
% ---

% When comparing the obtained result dict to the expected result dict, we can use:
% "==", then the tags and keys-value pairs must match
% "=",  then the tags must unify and key sets must be equal and the values must unify for each key
% There should be a way to "==" while disregarding the tag.

:- debug(partition_freely).

:- begin_tests(partition_freely).

const(list1,[silent,puffy,left,damaged,fascinated,deafening,wistful,whip,nest,inquisitive,imperfect,jog,unwieldy,provide,locket,reign]).

% comes_in_order(Sublist,List).
% perform == and \== instead of unifications, although we just compare ground terms here

comes_in_order([],_).
comes_in_order([X|RestSublist],[Y|RestList]) :- X == Y, !, comes_in_order(RestSublist,RestList).
comes_in_order([X|RestSublist],[Y|RestList]) :- X \== Y, !, comes_in_order([X|RestSublist],RestList).

test("partition an empty list", true(R == Exp)) :-
   partition_freely(([X,X]>>true),[],p,R),
   Exp = p{}.

test("partition using the 'identical' function", true(R == Exp)) :-
   partition_freely(([X,X]>>true),[1,4,5,3,2,3,1,1],p,R),
   Exp = p{1:[1,1,1],2:[2],3:[3,3],4:[4],5:[5]}.

test("not a real partition: partition randomly between 3 partitions") :-
   const(list1,List),
   partition_freely([_,Key]>>random_member(Key,[1,2,3]),List,_,R),
   debug(partition_freely,"Random result: ~q\n",R).

test("partition by atom length", true(R == Exp)) :-
   const(list1,List),
   partition_freely([A,Key]>>atom_length(A,Key),List,p,R),
   debug(partition_freely,"Partitioned by length: ~q\n",R),
   Exp = p{3:[jog],4:[left,whip,nest],5:[puffy,reign],6:[silent,locket],7:[damaged,wistful,provide],8:[unwieldy],9:[deafening,imperfect],10:[fascinated],11:[inquisitive]}.

% Below, note the "modulo values" passed to the partitioning function
% as a "Prolog closure" (a compound term that is a predicate call with
% some leftmost arguments filled).

% Also note that the prolog interpreter will have to find the partition
% predicate predicates in *this plunit module* from *outside this plunit
% module*, so precede them with the module name built according to plunit
% conventions (awkward, there should be a unique runtime-valid identifier
% instead)

test("partition by atom code sum modulo 1", true(R == Exp)) :-
   partition_const_list_of_atom_by_code_sum_modulo_m(1,R,Tag),
   Tag = p,
   Exp = p{0:[silent,puffy,left,damaged,fascinated,deafening,wistful,whip,nest,inquisitive,imperfect,jog,unwieldy,provide,locket,reign]}.

test("partition by atom code sum modulo 4", true(R == Exp)) :-
   partition_const_list_of_atom_by_code_sum_modulo_m(4,R,Tag),
   Tag = p,
   Exp = p{0:[whip,jog],1:[deafening,unwieldy,provide,reign],2:[puffy,fascinated,wistful,nest,inquisitive,locket],3:[silent,left,damaged,imperfect]}.

test("partition by atom code sum modulo 11", true(R == Exp)) :-
   partition_const_list_of_atom_by_code_sum_modulo_m(11,R,Tag),
   Tag = p,
   Exp = p{0:[whip,inquisitive],1:[wistful,jog,unwieldy],2:[nest,imperfect,provide],3:[damaged],4:[puffy,locket],5:[deafening,reign],6:[silent],8:[fascinated],9:[left]}.

test("partition by length modulo 5", true(R == Exp)) :-
   partition_const_list_of_atom_by_length_modulo_m(5,R,Tag),
   Tag = p,
   Exp = p{0:[puffy,fascinated,reign],1:[silent,inquisitive,locket],2:[damaged,wistful,provide],3:[jog,unwieldy],4:[left,deafening,whip,nest,imperfect]}.

% Helpers

verify_internal_partition_ordering(OriginalList,PartitionDict) :-
   dict_pairs(PartitionDict,_,Pairs),
   maplist({OriginalList}/[_Key-Partition]>>comes_in_order(Partition,OriginalList),Pairs).

partition_const_list_of_atom_by_code_sum_modulo_m(M,R,Tag) :-
   const(list1,List),
   partition_freely(plunit_partition_freely:partition_by_code_sum_modulo_m(M),List,Tag,R),
   debug(partition_freely,"Partitioned by code sum modulo ~d: ~q",[M,R]),
   (verify_internal_partition_ordering(List,R) -> debug(partition_freely,"Order ok",[]) ; (debug(partition_freely,"Order NOT ok",[]),fail)).

partition_const_list_of_atom_by_length_modulo_m(M,R,Tag) :-
   const(list1,List),
   partition_freely(plunit_partition_freely:partition_by_length_modulo_m(M),List,Tag,R),
   debug(partition_freely,"Partitioned by length modulo ~d: ~q",[M,R]),
   (verify_internal_partition_ordering(List,R) -> debug(partition_freely,"Order ok",[]) ; (debug(partition_freely,"Order NOT ok",[]),fail)).

% Partitioning predicates called by partition_freely/4

partition_by_code_sum_modulo_m(Modulo,Atom,Key) :-
   atom_codes(Atom,Codes),
   sum_list(Codes,CodesSum),
   Key is CodesSum mod Modulo.

partition_by_length_modulo_m(Modulo,Atom,Key) :-
   atom_length(Atom,Length),
   Key is Length mod Modulo.

:- end_tests(partition_freely).
