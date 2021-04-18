# Some notes taken while calling C from Prolog

The SWI-Prolog reference manual at [Foreign Language Interface](https://eu.swi-prolog.org/pldoc/man?section=foreign) is 
relatively difficult to navigate, so I will copy information to here, while taking a look at the `.h` file
in `$DISTRO/swiplexe_8.3.20/lib/swipl/include/SWI-Prolog.h`

Not that if you search for C procedures in the SWI Manual web interface you get the entries under
"The Foreign Include File", but if you click on any embedded text in the link, you get an entry under
[Analysing Terms via the Foreign Interface](https://eu.swi-prolog.org/pldoc/man?section=foreign-term-analysis).

## Allocating a new "term reference"

`term_t` is the reference to an empty cell, i.e. the reference to an unbound variable. 

```
PL_EXPORT(term_t) PL_new_term_refs(int n);
PL_EXPORT(term_t) PL_new_term_ref(void);
PL_EXPORT(term_t) PL_copy_term_ref(term_t from);
PL_EXPORT(void) PL_reset_term_refs(term_t r);
```

- [PL_new_term_ref](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_term_ref%27%29):
  Return a fresh reference to a term. The reference is allocated on the local stack.
  Allocating a term reference may trigger a stack-shift on machines that cannot use sparse
  memory management for allocation of the Prolog stacks. The returned reference describes a variable.
- [PL_new_term_refs](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_term_refs%27%29):
  Return n new term references. The first term reference is returned. The others are t+1, t+2, etc. 
- [PL_copy_term_ref](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_copy_term_ref%27%29):
  Create a new term reference and make it point initially to the same term as from. This function 
  is commonly used to copy a predicate argument to a term reference that may be written.
- [PL_reset_term_refs](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_reset_term_refs%27%29)


`PL_reset_term_refs` says:  Note that returning from the foreign context to Prolog will reclaim all 
references used in the foreign context. This call is only necessary if references are created inside
a loop that never exits back to Prolog. 

## Allocating a new atom

```
PL_EXPORT(atom_t) PL_new_atom(const char *s);
PL_EXPORT(atom_t) PL_new_atom_nchars(size_t len, const char *s);
PL_EXPORT(atom_t) PL_new_atom_wchars(size_t len, const pl_wchar_t *s);
PL_EXPORT(atom_t) PL_new_atom_mbchars(int rep, size_t len, const char *s);
```

- [PL_new_atom](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom%27%29):
   Return an atom handle for the given C-string.
- [PL_new_atom_nchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom_nchars%27%29):
  Create a new atom as `PL_new_atom()`, but using the given length and characters. If `len` is `(size_t)-1`,
  it is computed from s using `strlen()`.
- [PL_new_atom_wchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom_wchars%27%29): 
- Create atom from wide-character string as `PL_new_atom_nchars()` does for ISO-Latin-1 strings.
  If s only contains ISO-Latin-1 characters a normal byte-array atom is created. If `len` is
  `(size_t)-1`, it is computed from `s` using `wcslen()`.
- [PL_new_atom_mbchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_atom_mbchars%27%29)
  This function generalizes `PL_new_atom()` and `PL_new_atom_nchars()` while allowing for multiple encodings.
  The `rep` argument is one of `REP_ISO_LATIN_1`, `REP_UTF8` or `REP_MB`. If `len` is `(size_t)-1`,
  it is computed from `s` using `strlen()`.

## Extracting an atom from a term

```
PL_EXPORT(int) PL_get_atom(term_t t, atom_t *a) WUNUSED;
```

- [PL_get_atom](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_get_atom%27%29): 
  If t is an atom, store the unique atom identifier over a. See also PL_atom_chars() and 
  PL_new_atom(). If there is no need to access the data (characters) of an atom, it is
  advised to manipulate atoms using their handle. As the atom is referenced by t, 
  it will live at least as long as t does. If longer live-time is required, the atom 
  should be locked using PL_register_atom().

## Extracting a string from an atom

These return a point to array:

```
PL_EXPORT(const char *) PL_atom_chars(atom_t a);
PL_EXPORT(const char *) PL_atom_nchars(atom_t a, size_t *len);
PL_EXPORT(const wchar_t *) PL_atom_wchars(atom_t a, size_t *len);
```

- [PL_atom_chars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_atom_chars%27%29)
  Return a C-string for the text represented by the given atom. The returned text will not be 
  changed by Prolog. It is not allowed to modify the contents, not even‘temporary' as the string may 
  reside in read-only memory. The returned string becomes invalid if the atom is garbage collected.
  Foreign functions that require the text from an atom passed in a term_t normally use
  `PL_get_atom_chars()` or `PL_get_atom_nchars()`.
- [PL_atom_nchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_atom_nchars%27%29)
  Extract the text and length of an atom.
- [PL_atom_wchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_atom_wchars%27%29)
  Extract characters from a wide-character atom. Succeeds on any atom marked as "text". If
  the underlying atom is a wide-character atom, the returned pointer is a pointer into
  the atom structure. If it is an ISO-Latin-1 character, the returned pointer comes from
  Prolog's "buffer ring". 

These set a pointer to array and return 0 or 1:

```
PL_EXPORT(int) PL_get_atom_chars(term_t t, char **a) WUNUSED;
PL_EXPORT(int) PL_get_atom_nchars(term_t t, size_t *len, char **a) WUNUSED;
```

- [PL_get_atom_chars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_get_atom_chars%27%29),
  [PL_get_atom_chars](https://eu.swi-prolog.org/pldoc/man?CAPI=PL_get_atom_chars)
  If t is an atom, store a pointer to a 0-terminated C-string in s. It is
  explicitly not allowed to modify the contents of this string. 
  Some built-in atoms may have the string allocated in read-only memory,
  so "temporary manipulation" can cause an error.
- [PL_get_atom_nchars](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_get_atom_nchars%27%29),
  [PL_get_atom_nchars](https://eu.swi-prolog.org/pldoc/man?CAPI=PL_get_atom_nchars)


## Allocating a new functor (i.e. a structure with a functor name and an arity)

```
PL_EXPORT(functor_t)    PL_new_functor(atom_t f, int a);
PL_EXPORT(functor_t)    PL_new_functor_sz(atom_t f, size_t a);
```

- [PL_new_functor](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_new_functor%27%29)
  Returns a functor identifier, a handle for the name/arity pair. The returned handle is valid
  for the entire Prolog session.
- PL_new_functor_sz remains undocumented but should be same as above, just using size_t as
  type of the arity

## Analyzing a functor

```
PL_EXPORT(atom_t) PL_functor_name(functor_t f);
PL_EXPORT(int) PL_functor_arity(functor_t f);
PL_EXPORT(size_t) PL_functor_arity_sz(functor_t f);
```

- [PL_functor_name](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_functor_name%27%29)
  Return an atom representing the name of the given functor.
- [PL_functor_arity](https://eu.swi-prolog.org/pldoc/doc_for?object=c%28%27PL_functor_arity%27%29)
  Return the arity of the given functor.
- PL_functor_arity_sz remains undocumented but should be same as above, just using size_t as
  return type

## Incrementing/Decrementing an atom's reference count

To free an atom created by the above (i.e. to maybe make it eligible for garbage collection, in
case its reference count has gone to 0)

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

How does this work concretely?

When your C function gets called, it is passed a `term_t`. `term_t` is a reference to a graph under
management by the Prolog engine, i.e. a "Prolog term". In order to communicate information to the
caller the `term_t` should be of type "variable" (i.e. an empty cell, an unbound variable, a 
representation of lack of knowledge). We don't care how that variable is named (maybe `X`
or `_`) and actually can't find out. It could not even have a name. No matter: we just fill it with
the newly allocated atom. If I understand correctly, this will increase the reference count of the
atom by 1. Just before return, we need to decrement the atom's reference count by one, thus allowing
the Prolog engine to garbage collect the atom properly once it has been finally dereferenced!






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

