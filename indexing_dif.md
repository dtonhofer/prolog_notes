Notes made while reading "[Indexing `dif/2`](https://arxiv.org/abs/1607.01590v1)" (2016-07: Ulrich Neumerkel, Stefan Kral).

This paper raises a lot of further paths for exploration. Also about the semantics of Prolog.

`member/2` is really the drosophila of Logic Programming.

Predicate discussed:

- [`member/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=member/2)
- [`memberchk/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=memberchk/2)
- [`->`](https://eu.swi-prolog.org/pldoc/doc_for?object=(-%3E)/2)
- [`*->`](https://eu.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2)
- [`dif/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2)

The SWI-Prolog manual is a bit bare!

Exercises:

- What is the general behaviour of `->` and `*->`. Write some unit test.

TODO:

- More on floundering for [\+](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%2B)/1)?
  From "Declarative Diagnosis of Floundering in Prolog": 
  > Many logic programming languages have delay primitives which allow coroutining. This introduces a class  
  > of bug symptoms — computations can flounder when they are intended to succeed or finitely fail. For concurrent
  > logic programs this is normally called deadlock.


- [Declarative Diagnosis of Floundering in Prolog](https://arxiv.org/abs/0711.0048) (Lee Naish, 2012, in: _Proceedings of the Thirty-Fifth Australasian Computer Science Conference (ACSC 2012), Melbourne, Australia_)
- Lee naish has bunch of Good Stuff at the arxiv: [Query](https://arxiv.org/search/?query=lee+naish&searchtype=all&source=header). 
