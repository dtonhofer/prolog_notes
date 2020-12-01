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

