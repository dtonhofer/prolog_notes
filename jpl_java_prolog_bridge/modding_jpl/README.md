# Modding JPL - Work in progress

So I want to go a bit through the code for jpl.pl, the SWIPL-Java interface-via-JNI.

This is a good exercise.

The code is also hard to read and needs a bit of polish.

jpl.pl is also a gigantic file, but people are ok with that. I guess they have better editor support
than I do currently. My preference would be to cut jpl.pl up into a number of thematic subfiles (if these
can be identified) but that is not desired at the present time.

Step #1: Move all the in-line exception texts out to a common block.

I wanted to move all the excpetion texts to a separate file, but this is not desired at the moment.
So the exception texts stay in-file (one day, these will have to be moved out of the source,
as is done with Java strings: [Java Internationalization](https://dzone.com/articles/a-beginners-guide-to-java-internationalization).

For now, I am happy enough to move the "exception descriptors" to the bottom of `jpl.pl`.

This idea is explained in [Good idea: Avoiding cleartext errors littering you code, aka "Throwing in Style"](https://github.com/dtonhofer/prolog_notes/tree/master/swipl_notes/about_exceptions#good-ideas)

After detail work this has been attained (yeah!!) with tow generations of update:

- Update one, which was still a bit rough because only exception texts were selected by keywords,
  which is both a bit weak and still demands that you look up the keyword manually
- Update two which selects the whole exception to throw with a pair of terms, which can be
  formed so that one can find out what is supposed to be thrown just be looking at the code
  plus it's still configurable because the text and actual exception term can be tuned
  in a separate "data clause". Plus there is code to test that the throwm/2 calls are
  all actually correct. See https://github.com/SWI-Prolog/packages-jpl/pull/80#issuecomment-668484491

TODO:

- Rewrite the Perl script that extracts the throwme/2 calls for testing in Prolog
- Find a way to set this up as a test case (if so wanted).
- Update the exception-throwing code some more to the latest version of "throwing in style",
  including comments, which is here: [`throwme_nonmodular.pl`](https://github.com/dtonhofer/prolog_notes/blob/master/code/heavycarbon/support/throwme_nonmodular.pl)
- Maybe integrate the testing code into test.jpl

- Start updating the DCG!

lso, keep checking the [JPL issues](https://github.com/SWI-Prolog/packages-jpl/issues)

