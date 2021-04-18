
The SWI-Prolog reference manual at [Foreign Language Interface](https://eu.swi-prolog.org/pldoc/man?section=foreign) is 
relatively difficult to navigate, so I will copy information to here, while taking a look at the `.h` file
in `$DISTRO/swiplexe_8.3.20/lib/swipl/include/SWI-Prolog.h`

**Allocating a new atom**

```
PL_EXPORT(atom_t) PL_new_atom(const char *s);
PL_EXPORT(atom_t) PL_new_atom_nchars(size_t len, const char *s);
PL_EXPORT(atom_t) PL_new_atom_wchars(size_t len, const pl_wchar_t *s);
PL_EXPORT(atom_t) PL_new_atom_mbchars(int rep, size_t len, const char *s);
```

- [PL_new_atom](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom%27%29):
   Return an atom handle for the given C-string.
- [PL_new_atom_nchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom_nchars%27%29)
- [PL_new_atom_wchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom_wchars%27%29)
- [PL_new_atom_mbchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom_mbchars%27%29)

To free an atom created by the above (i.e. to maybe make it eligible for garbage collection, in
case its reference count has gone to 0):

**Incrementing/Decrementing an atom's reference count**

```
PL_EXPORT(void) PL_unregister_atom(atom_t a);
PL_EXPORT(void) PL_register_atom(atom_t a);
```

From [Atoms and atom garbage collection](https://eu.swi-prolog.org/pldoc/man?section=atomgc):

- [PL_register_atom](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_register_atom%27%29):
  Increment the reference count of the atom by one.
  `PL_new_atom()` performs this automatically, returning an atom with a reference count of at least one.
- [PL_unregister_atom](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_unregister_atom%27%29):
  Decrement the reference count of the atom. If the reference count drops below zero, an assertion error is raised.

**Getting/Using the special nil (i.e. the empty list): `[]`**

```
PL_EXPORT(int) PL_get_nil(term_t l);  // compiler warns if result is unused
PL_EXPORT(int) PL_put_nil(term_t l);
PL_EXPORT(int) PL_unify_nil(term_t l); // compiler warns if result is unused
PL_EXPORT(int) PL_unify_nil_ex(term_t l); // compiler does not warn if result is unused but should (?)
```

- [PL_get_nil](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_get_nil%27%29)
- [PL_put_nil](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_put_nil%27%29)
- [PL_unify_nil](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_unify_nil%27%29)
- [PL_unify_nil_ex](https://eu.swi-prolog.org/pldoc/doc_for?object=c(%27PL_unify_nil_ex%27)):
  Wrapper function. As PL_unify_nil(), but raises a type error if t is not a variable, list-cell or the empty list.
  See [Convenient functions to generate Prolog exceptions](https://eu.swi-prolog.org/pldoc/man?section=cerror)

