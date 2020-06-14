Thw whole manual

- [Java Native Interface Specification Contents](https://docs.oracle.com/en/java/javase/14/docs/specs/jni/) (Java 14)

Specific headings

- [Chapter 4: JNI Functions](https://docs.oracle.com/en/java/javase/14/docs/specs/jni/functions.html)
- [FindClass method](https://docs.oracle.com/en/java/javase/14/docs/specs/jni/functions.html#findclass)

SWI-Prolog JPL page

[JPL](https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/jpl.html%27))

The canonical documentation site is http://jpl7.org/ "which subsumes the wiki".

The pointers given at the top of the page need to be swapped.

https://docs.oracle.com/en/java/javase/14/docs/specs/jni/types.html

The following should not happen:

The JPL module regards SWI Prolog strings as lists. Use atoms instead:

```
?- jpl_call('java.lang.System','getProperty',["java.version"],V).
ERROR: Type error: `method_params' expected, found `["java.version"]' (a list) (not all actual parameters are convertible to Java values or references)
ERROR: In:
ERROR:   [11] throw(error(type_error(method_params,...),context(...,'not all actual parameters are convertible to Java values or references')))
ERROR:   [10] jpl:jpl_call('java.lang.System',getProperty,["java.version"],_23222) at /usr/local/logic/swipl/lib/swipl/library/jpl.pl:307
ERROR:    [9] <user>
```

