# Bibliography of the SWI-Prolog manual: How to fix?

https://swi-prolog.discourse.group/t/fixing-adding-a-bibliogaphy-in-the-swi-prolog-manual/3311/3

Moving something to the biblipgraphy:

So…

   - I have modified `man/extensions.doc` with a `\cite` entry.
   - I have changed `man/pl.bib` in my github clone (fun to read:
     [How to not mess up your bibliographies with Bibtex: Manual curation is the only reliable approach to reference management](https://clauswilke.com/blog/2015/10/02/bibtex/))
   - I have also managed to generate a `pl.bbl` file with the bibliography entries in TeX style and copied it to `man/gen/swipl.bbl`
     (as described in [How to obtain and use the .bbl file in my tex document for ArXiv submission](https://tex.stackexchange.com/questions/329198/how-to-obtain-and-use-the-bbl-file-in-my-tex-document-for-arxiv-submission/)?

However, rebuilding does not resolve the new `\cite` entry, and to boot the `pl.bbl` file contains a lot of entries now that were not in there before.

Clearly my work process is lacking in correctness.

How do I do it correctly?

---

jan says:

If you have installed the dependencies to build the PDF documentation, configure using 

`cmake -DBUILD_PDF_DOCUMENTATION=ON …`

and build it. That produces `man/SWI-Prolog-<version>.bbl` in the build directory. 
That creates the correct `.bbl`, only containing the entries that are actually used. 
`pl.bib` contains a lot of stuff that is (no longer) used. Please leave it in, it may become of use later

Copy `man/SWI-Prolog-<version>.bbl` to `../man/gen/swipl.bbl` (should result in a change according to `git diff`)

Important: in a build dir not setup for the PDF docs, make sure to delete `man/SWI-Prolog-<version>.bbl` before building
the HTML. Without this step this file is not copied.

Make sure to rebuild the HTML docs. One way is to delete `man/pldoc2tex` from the build dir as all generated LaTeX files
depend on that. Then run `ninja`.

If all is fine, commit the change into git.

Note that all this breaks a golden rule of version management: generated files should not be in a repository. The alternative
is to force the entire LaTeX suite as build dependency and always build the PDF. That is IMO an overkill, also because
`pl.bib` and citations do not change often.

---

This is a bit messy.

- Make sure you have installed package texlive-a4wide
- Make sure you have install packge epstopdf

There is a script referenced by `RUNTEX` that is hooked into the cmake processing run via:

```
    if(BUILD_PDF_DOCUMENTATION)
      add_custom_command(
          OUTPUT ${pkg}.pdf ${bbl}
          COMMAND ${RUNTEX} --pdf ${pkg}
          DEPENDS ${texdeps} ${depends}
          COMMENT "Generating ${pkg}.pdf")

      add_custom_target(
          ${pkg}.doc.pdf
          DEPENDS ${pkg}.pdf)
      add_dependencies(doc.pdf  ${pkg}.doc.pdf)
    elseif(bbl)
      add_custom_command(
          OUTPUT ${bbl}
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
                  ${CMAKE_CURRENT_SOURCE_DIR}/gen/${bbl} ${bbl}
          COMMENT "Copying pre-build LaTeX .bbl file")
    endif(BUILD_PDF_DOCUMENTATION)
```

`RUNTEX` is `$SWIDIR/man/runtex`

Unfortunately, if it has problems, nothing about the problems appears in the log generated by cmake.

For example, I had no epstopdf. If you run the above manually, you get a complaint. Not so if you run `cmake`. I do not really know why. 

The files changed are thus:

- `man/runtex` (script changes)
- `man/pl.bib` (bibliography entry changes)
- `man/extensions.doc` (reference changed)

So we copy these as needed.

Then we build!

../../swiprologpull.sh build docu withpdf

This indeed creates

./swipl-devel_original/build/man/SWI-Prolog-8.3.14.bbl

Disconcertingly, URLs may be nuked if they are in field "url". They must be in "note"!

e.g. this URL was retained, it was in `note`:

```
@inproceedings{clpb:Triska2016,
  author    = "Markus Triska",
  title     = "The {Boolean} Constraint Solver of {SWI-Prolog}:
               System Description",
  booktitle = "{FLOPS}",
  series    = "LNCS",
  volume    = 9613,
  year      = 2016,
  pages     = "45--61",
  note = "https://www.metalevel.at/swiclpb.pdf"
}
```

But this one is not, it was in `url`:

```
@unpublished{declarativeloops:2010,
  author    = {Neng-Fa Zhou},
  title     = {Declarative Loops and List Comprehensions for {Prolog}},
  year      = {2010},
  month     = {jan},
  day       = {25},
  url       = {http://www.sci.brooklyn.cuny.edu/~zhou/papers/loops.pdf}
}
```

OTOH, this one is live:

```
@Misc{stdprolog:98,
  author = "Jonathan Hodgson",
  title  = "Validation suite for conformance with Part 1 of the standard",
  year   = 1998,
  url    = "http://www.sju.edu/~jhodgson/pub/suite.tar.gz"
}
```

Copy `man/SWI-Prolog-<version>.bbl` to `../man/gen/swipl.bbl` (should result in a change according to `git diff`)

Ok so:

diff --side-by-side  ./swipl-devel_original/build/man/SWI-Prolog-8.3.14.bbl swipl-devel_forked/man/gen/swipl.bbl

Shows a lot of additional entries (they were not filtered?) and the new one.

Grep for PDFs in the built directory:

```
$ find . -name "*.pdf" | grep -v figs
./build/man/SWI-Prolog-8.3.14.pdf
./build/packages/pldoc/pldoc.pdf
./build/packages/pcre/pcre.pdf
./build/packages/cql/cql.pdf
./build/packages/nlp/nlp.pdf
./build/packages/zlib/zlib.pdf
./build/packages/paxos/paxos.pdf
./build/packages/redis/redis.pdf
./build/packages/plunit/plunit.pdf
./build/packages/table/table.pdf
./build/packages/archive/archive.pdf
./build/packages/libedit/libedit.pdf
./build/packages/protobufs/protobufs.pdf
./build/packages/clib/clib.pdf
./build/packages/jpl/jpl.pdf
./build/packages/tipc/tipc.pdf
./build/packages/readline/readline.pdf
./build/packages/utf8proc/utf8proc.pdf
./build/packages/http/http.pdf
./build/packages/cpp/pl2cpp.pdf
./build/packages/ssl/ssl.pdf
./build/packages/RDF/rdf2pl.pdf
./build/packages/sgml/sgml.pdf
./man/Makefile.pdf
```

