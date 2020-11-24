# SWI-Prolog dicts

This is companion information for the SWI-Prolog manual page [Dicts: structures with named arguments](https://eu.swi-prolog.org/pldoc/man?section=bidicts).

_Dicts_ are an SWI-Prolog-specific extension (at least for now, maybe they will find a way into the ISO standard one day).

They are the SWI-Prolog equivalent of "maps", "hashes", "hashmap", "dictionaries" or "association lists" in other programming languages.
The goal is to associate a (possibly complex) value to a key inside a container. The implementation of the containeris, however, fixed,
unlike in languages like Java for example, where one gets to choose the actual implementation to realize an instance of the
[`Map` interface](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Map.html).

For an alternative using a library (as opposed to a language built-in), take a look at
[`library(assoc)`: Association lists](https://eu.swi-prolog.org/pldoc/man?section=assoc)
written by Richard A. O'Keefe, L.Damas, V.S.Costa and Markus Triska.

There is an additional helper library:

[`library(dicts)`](https://eu.swi-prolog.org/pldoc/man?section=dicts) 

which "defines utilities that operate on lists of dicts, notably to make lists of dicts consistent by adding missing keys,
converting between lists of compounds and lists of dicts, joining and slicing lists of dicts."

(A pretty printer seems to be missing in that library. TODO!).

[Dict Equality and Unification](equality_and_unification.md)
