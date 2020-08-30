%%%%%%%%%%%%%%%
% This file should be run on the Prolog toplevel to load module files,
% load their unit test code, then execute the unit test code. Most of the
% file content is commented-out in Prolog fashion, except for some 
% instructions that do as described.
%
% Start SWI-Prolog in directory "code", then issue:
%
% [load_and_test_script].
% PL-Unit: string_conversion .............. done
% PL-Unit: atom_conversion .............. done
% PL-Unit: leveling ....... done
% PL-Unit: string_of_spaces ......... done
% PL-Unit: string_overwriting .. done
% All 46 tests passed
% true.
%%%%%%%%%%%%%%%

/*

How to do it:

Conventions 
===========

1) All my Prolog code is in Module files (instead of loose scripts and
   loose code to be loaded into the "user" module) unless  there is a good 
   reason for this not to be the case.

2) All the Prolog code is files found in a directory hierarchy like this:

   code/heavycarbon/${theme}/${module_basic_name}.pl   for effective code
   code/heavycarbon/${theme}/${module_basic_name}.plt  for unit test code
   
   where:

   - ${theme} is some general coding theme (e.g. "strings")
   - ${module_basic_name} is a recognizable string, but generally NOT the 
     module name. The module name is longer and looks more like the pathname.
   - "heavycarbon" is a string that come my Java code domain name, 
     "heavycarbon.name". Java has the correct idea about package naming - 
     basing it on the reverse domain name of the publisher to avoid name clashes
     in the (actually non-hierarchical) package name space. Let's imitate
     this here for module naming (but I will drop the TLD part ".name" because
     it's annoying)
            
   I will not refrain from creating lots of small modules and thus lots of 
   little files, although the Prolog tradition seems to be to create 
   few modules with (very) large files. This may be possible due to skillful
   editor support, I don't know (yet). 
      
3) Module declaration examples:
        
   Define module "heavycarbon_strings_leveling" and its exports in file
   "heavycarbon/strings/leveling.pl":

   |
   | :- module(heavycarbon_strings_conversion,[convert_to_string/2]).
   | 
   
   Define module "heavycarbon_strings_conversion" and its exports in file
   "heavycarbon/strings/conversion.pl": 

   |
   | :- module(heavycarbon_strings_leveling,[leveling_string/2]).
   |

 Load modules into Prolog via script file (i.e. via this file)
 =============================================================

 Directives (instruction starting with ':-') are used because we are in
 "script mode". Prolog with execute the directive as if you had typed it.
 
1) First, tell Prolog to add the directory "code" to its library search
   path. The following is used if directory "code" (as mentioned above)
   is the current directory. It just adds the current directory to the
   "library search path" 
   
   (here, uncommented text so that **this very file** can be run in Prolog):    
*/

   :- assertz(file_search_path(library,'.')). 

/*

2) Then instruct Prolog to load your modules, where you indicate the
   **module file**, not the **module name**, as argument to ue_module/1. 
   The argument can be a term or an atom/string.

   The "basename" of the filename, it. the filename stripped off the 
   directory path (here in variable ${basename}) is given to Prolog but
   without the ".pl" suffix. (This doesn't work for ".plt" files though).

   A listing of all the "use_module" instructions in code files can be
   had using:

   | 
   | grep -R --no-filename use_module . 2>/dev/null | sort | uniq
   |

   Here are the currently valid use_module/1 calls found in source code:
  
*/

:- use_module(library('heavycarbon/strings/conversion.pl')).
:- use_module(library('heavycarbon/strings/stringy.pl')).
:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).
:- use_module(library('heavycarbon/strings/string_overwrite.pl')).
:- use_module(library('heavycarbon/strings/justify.pl')).
:- use_module(library('heavycarbon/support/utils.pl')).
:- use_module(library('heavycarbon/utils/between_with_step.pl')).
:- use_module(library('heavycarbon/utils/openlist_append.pl')).
:- use_module(library('heavycarbon/utils/clashfree_id_selection.pl')).
:- use_module(library('heavycarbon/utils/difflist_length.pl')).
:- use_module(library('heavycarbon/utils/randomly_select.pl')).
:- use_module(library('heavycarbon/utils/replace0.pl')).
:- use_module(library('heavycarbon/utils/rotate_list.pl')).
:- use_module(library('heavycarbon/utils/splinter0.pl')).
:- use_module(library('heavycarbon/utils/vector_nth0.pl')).
:- use_module(library('heavycarbon/utils/vector_replace0.pl')).
 
/*

   An alternative notation, though I prefer using an atom as relative
   pathname and making the suffix explicit (default behaviours made to
   help interactivity in non-interactive situations only lead to problems
   later)

   | 
   | % use a non-atom term; can't use a ".pl" suffix here because that 
   | % would look like dict operation
   | :- use_module(library(heavycarbon/strings/conversion)). 
   |
   | % use an atom without giving suffix
   | :- use_module(library('heavycarbon/strings/conversion')).
   |

   After one or all of the above have been run, the exported predicates are 
   visible within the importing module (on the toplevel, that would be 
   [user]).

   If you need to load file that have no module declaration into your module:

   | 
   | :- include(library('heavycarbon/strings/meta_helpers_nonmodular.pl')).
   | 

   load_files/1 could alos be once, but then you can load the file only
   once!

   As for use_module/2, the relative filename is relative to the library
   search path if you tag (enclose) the filename with library/1.

   See https://eu.swi-prolog.org/pldoc/man?predicate=include/1
   See https://eu.swi-prolog.org/pldoc/doc_for?object=load_files/2
  
3) For each module, unit test code can be found in a file with the same basename
   as the module file, but with the extension ".plt". (Note that github does not 
   recognizes .plt files as Prolog files :-( and this messes up syntax coloring.)

   ** IMHO, it would be better to extend the "module" declaration with an option
   ** to indicate the files that contain test code. I have to think about this...

   Inside the ".plt" files, the modules under test are loaded the same way as 
   indicated above:

   |
   | :- use_module(library('heavycarbon/utils/openlist_append.pl')).
   |
  
   Directives can now be issued to load all tests from module-associated ".plt"
   files, and then run them: 
 
   (Is there a way to dynamically list the valid .pl/.plt associations?)
 
*/

   :- load_test_files([]).
   :- run_tests().
   
/*   

If you just want to run unit test code for a specific module manually:
---------------------------------------------------------------------- 

1) Make sure the library search path has been extended by issuing this
   at the Prolog Toplevel (the same as earlier, not in form of a directive):

   ?- assertz(file_search_path(library,'.')). 
   
   You can examine the currently configured library search path via:
   
   ?- file_search_path(library,X).
   X = app_config(lib) ;
   X = swi(library) ;
   X = swi(library/clp) ;
   X = '/home/foouser/.local/share/swi-prolog/pack/sldnfdraw/prolog' ;
   X = pce('prolog/lib').

   USING THE INITIALIZATION FILE init.pl
   - - - - - - - - - - - - - - - - - - -
   
   It is better to have the above, not with the path '.' but with the
   actual path, configured via a directive in the user's initialisation file so
   that the library extension operation s performed at startup fo SWI Prolog.
   
   The file containing the directive might possibly be in directory:
   
   '$HOME/.config/swi-prolog/'
   
   That's the case on my system (Linux/KDE). Note that .config is used by KDE,
   it's full of stuff! And the 'swi-prolog' subdirectory already contains some cache
   files, too.
   
   To find the directory on your system, run this at the Prolog Toplevel:
   
   ?- absolute_file_name(app_config(.), Dir, [file_type(directory)]).
      
   See also https://eu.swi-prolog.org/pldoc/man?section=initfile   
 
   Once you know the location (and have created the directory as needed), create
   a file called `init.pl` in that directory. Then add the following for example,
   to complement the library search path and immediately load in a library on that
   search path:
   
   |
   |  :- assertz(file_search_path(library,'/home/foouser/my/code/repo')). 
   |  :- use_module(library('heavycarbon/terms/clashfree_id_selection.pl')).
   |
   
   Immediately less typing required!
   
   Also take a look at a sample user initialization file that comes with the
   distribution:
   
   $SWIPL_DISTRO_DIR/lib/swipl/customize/init.pl
 
2) Consult the ".plt" file, which is assumed to have the required 

   |
   | :- use_module(library(heavycarbon/strings/conversion)).
   | 
   
   directives at its top.
   
   ?- ['heavycarbon/strings/leveling.plt'].
   true.

3) Run tests, see https://eu.swi-prolog.org/pldoc/doc_for?object=run_tests/0

   ?- run_tests.
   % PL-Unit: leveling ....... done
   % All 7 tests passed
   true.

4) After a code change, it may be sufficient to issue

   ?- make.

   to re-consult changed files.
   See https://eu.swi-prolog.org/pldoc/doc_for?object=make/0.

   Sometimes Prolog gets confused and you have to quit-restart.
   This is why it's good to keep command sequences in script files.

   Calling make. will also sometimes re-run the test as if you had executed
   run_all_tests. But not always (not sure when).

If you want to run unit test code for a loaded modules
------------------------------------------------------ 

If you have issued "use_module/2" calls on the Prolog Toplevel, the ".plt" files 
corresponding to the module files can be automatically hoovered up as described
above, just run the directives manually:

?- load_test_files([]).
?- run_tests().

Debug-printing instruction in unit test code
--------------------------------------------

By default, these should be off.

It would be good to have a script which extracts the "debug topics" which may switched
on at the Prolog toplevel. 

*/   

