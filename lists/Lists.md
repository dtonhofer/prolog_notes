# The List!

Prolog has no array or other opaque "list-like" or "map-like" opaque datastructure.

(Except maybe SWI-Prolog's dicts?)

(Why are there no (non-mutable) opaque datastructures? Is it historical? Clojure has a pageful. Is there a consistent way to add them? A typed Prolog would certainly need those.)

There is only the list, which is not really a datastructure, but a convention (and an implicit contract, to be uheld by the caller and to be assumed by the caller) on how a term is non-locally structured.

It could well be that the list you get passed is not really a list a few nodes down the backbone. Problems ensue.

Checking for "listness" is done by [is_list/1](https://www.swi-prolog.org/pldoc/doc_for?object=is_list/1), which also fails if the list is cyclic.

Prolog could have a "contract marker" on terms and variables saying "it was verified that this is a list (with the following properties)" (and othe rstatements about the term: "this is a set" (set by the to-set function, "this is an ordset" etc.). Would that make sense.

## Vocabulary

- Head
- Tail
- etc.

## Cases

Lists can be plain or special stucture:

- Plain
- Cyclic
- Lazy

There is also a particular data structure which keeps the end of a list "fresh" and directly accessible so that append operations are cheap:

- List Differences (aka. Difference Lists)

Do not confuse the conjunction `(x,y,z)` with the list `[x,y,z]`. (In Clojure anything printed `(x,y,z)` is a `Seq`, which is the common interface to "list-like objects")

List can be used in the following roles, with the list role appropriately changing even in the same clause (e.g. member/2 makes sense in a bag-like list and memberchk/2 in a set-like list, but you can have both with an nth0/3 afterwards on the same list)

**Lists as Lists**

- Just a container of stuff with a complete ordering relation
- Operations:
   - reverse/2 (lfold a list with [] and cons), O(n) but lots of memory accesses
   - permutation/2
   - append/3
   - append/2
   - prefix/2
   - nextto/3

**Lists as Stacks**

- Most efficient use.
- Add/Remove tip via Unification: "Destructuring" and (I would say) "Constructuring"?
- Push/Pop operation at the tip of a list (in Perl, operations at the tip of an array or list are called shift/unshift; push/pop happens at the end of an array or list).

**Lists as Arrays**

- Access by index
- Getting at element i is O(i) because one has to search linearly down the backbone
- There is no replace, let's write one.
- Operations
   - nth0/3
   - nth1/3
   - nth0/4
   - nth1/4
   - vector_nth0
   - No vanilla replace0 or vector_replace0 ... need to add it.
   
**Lists as Maps (Key-Value Sets)**

- List items have particular structure, like Key-Value ("a pair")
- Searching is linear
- zip/unzip or assemble/dissassemble list
- Look at dicts for an alternative structure based on search trees

**Lists as Queues**

- Append, Remove at the end is inefficient, look at Difference Lists
- Operations
   - last/2

**Lists as Bags**

- Lists contain arbitrary stuff (terms), possible multiple copies of the same term.
- There is no type enforcement on list contents, so watch out!
- Operations
   - member/2
   - select/3
   - selectchk/4

**Lists as Sets**

- The implicit contract is that the list shall only contain one single copy of any term distinguishable (generally by the standard order of terms) 
- ordsets
- Operations
   - membercheck
   - is_set/1
   - list_to_set/2
   - intersection/3
   - union/3
   - subset/2
   - subtract/3
- More efficient: library(ordset)

**Special sauce**

- flatten/2
- max_member/2
- min_member/2
- proper_length/2
- same_length/2

