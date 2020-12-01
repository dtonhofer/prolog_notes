# F-Logic

This is implemented in [Flora-2](http:⁄⁄flora.sourceforge.net⁄) / Ergo-Lite running on top of [XSB Prolog](http://xsb.sourceforge.net/). 
Currently (October 13, 2018) at Version 2.0 ("Pyrus nivalis"), 

## Reading

### F-Logic: A Higher-Order Language for Reasoning about Objects, Inheritance, and Scheme

   - 1989-06 
   - Appears in: ACM SIGMOD Record, June 1989
   - [Reference at ACM](https://dl.acm.org/doi/10.1145/66926.66939)
   - [Paper at researchgate](https://www.researchgate.net/publication/2797469_F-Logic_A_Higher-Order_Language_for_Reasoning_about_Objects_Inheritance_and_Scheme)

> Object-oriented approach to databases has generated considerable interest in the community in the past few years (i.e in the 80s).
> Although the very term “object-oriented” is loosely defined, a number of concepts have been identified as the most salient 
> features of that approach. According to [4,39,9,43] and a number of other surveys, these concepts are: complex objects, object
> identity, methods, and inheritance.
>
> One of the reasons for the interest in object-oriented database languages is that they show promise in overcoming the, so 
> called, impedance mismatch between programming and database languages. Meanwhile, a different, deductive, approach has gained
> enormous popularity both in databases and programming anguages. Since logic programming can be used as a full-fledged 
> computational formalism as well as adatabase specification language, proponents of the deductive approach have also argued 
> that it is capable of overcoming the aforesaid mismatch problem. However, in their present form both approaches have shortcomings.
> The main problem with the object-oriented approach is the lack of formal semantics which is traditionally considered to be very
> important in databases. On the other hand, deductive databases normally use flat data model and do not support object identity and
> data abstraction. It therefore can be expected that combining the two paradigms will yield significant benefits. (...)
> 
> In this paper we propose a formalism, called Frame Logic (abbr. F-logic), which is a full-fledged logic achieving all of the goals 
> listed above and, in addition, is suitable for defining and manipulating database schema. Our work has also implications for the
> frame-based languages in AI, which are essentially scaled down versions of complex objects with identity, inheritance, and deduction. 
> It is this connection that the name “F-logic” was derived from.

Example from that paper:

**Facts**

   1. `faculty:bob[name -> "Bob",age -> 40,works -> dept:cs1[dname -> CS", mngr -> empl:phil]]`
   2. `faculty:mary[name -> "Mary",age -> 30,friends -> {bob,sally}, works -> dept:cs2[dname->"CS"]]`
   3. `assistant:john[name -> "John",works->cs1[dname->"CS"]]`
   4. `student:sally[age->midaged]`

**General Class Information**

   5. `faculty[supervisor->faculty,age->midaged]`
   6. `student[age->young]`
   7. `empl[supervisor->empl]`

**Rules**

   8. "employee’s supervisor is the manager of the department the employee works in."

```
E[supervisor->M] <= 
    empl:E[works->dept:D[mng->empl:M]]
```

   9. A rule defining a method: for each person, `X`, the method `children` is a function which for a given argument, `Y`, 
      returns a set of persons containing all the children common to `X` and `Y`. Notice that the term `children(Y)`
      appears in the same clause in two different roles: in the head it is in the “attribute” position, where it denotes
      the aforesaid method, and it is in the “object” position in the body, where it denotes the id of the object whose
      content is the set of children of `Y`. Thus, any ground instance of this term, say `children(mary)`, has
      two different roles: it denotes the object representing all mary’s children and also a function which, for each person,
      returns a (possibly empty) set of all children mary has with that person. Thus, in F-logic, the same syntactic 
      term may denote different things, depending on the context it appears in. This feature allows one to pose 
      meta-queries about the objects, such as “retrieve a set of all objects which represent the labels defined for a certain object”.

```
X[children(Y)->{Z}] <= 
    person:Y[children_obj->children(Y)[members->{person:Z}]],
    person:X[children_obj->children(X)[members->{person:Z}]].
```

**Queries**

Query (10) asks for the father of mary’s child sally

   10. `mary[children(Y)->{sally}]?`

Query (11) requests all children mary has with phil. 

   11. `mary[children(phil)->{Z}]?`

Query (12) is requesting information about all middle aged employees working for the `“CS”` departments.
Particularly, for each such employee, it is requesting the supervisor, age, and the department. 

   12. `empl:X[supervisor->Y,age->midaged:Z,works->D[dname->"CS"]]?`

The expected answer to this query is:

   13. `bob[supervisor -> ⊤, age -> 40, works -> cs1]`
   14. `mary[supervisor -> faculty, age -> 30, works -> cs2]`

`⊤` means "inconsistent information", it is the "meaningless object" or "class with no instances"):
 
If the restriction `midaged : Z` in query (12) were dropped, then the following clause will be retrieved as well:

   15. `john[supervisor -> phil, age -> young, works -> cs1]`


