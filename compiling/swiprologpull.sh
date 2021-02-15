#!/bin/bash

# ===========================================================================
# This is free and unencumbered software released into the public domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
#
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# For more information, please refer to <http://unlicense.org/>
# ===========================================================================
# swiprologpull.sh
#
# I don't like to think when building, so here is a bash shell script to
# perform:
#
# System-level installation:
#
# - Clone the latest original SWI-Prolog distribution including submodules
#   from GitHub to the local machines, and build it for system-level
#   installation. You then just run "ninja install" (and change the PATH in
#   bashrc).
#
# Documentation maintenance:
#
# - Clone a fork of SWI-Prolog from my personal GitHub account to the local
#   machine for documentation maintenance, copy a selected set of files from
#   that clone to a clone of the original SWI-Prolo distribution, build that
#   and see whether the documentation looks right.
#
# JPL package maintenance:
#
# - Clone a fork of the "JPL package" (SWI-Prolog/Java bridge) from my personal
#   GitHub account to the local machine, copy a selected set of files from
#   that clone to a clone of the original SWI-Prolo distribution, build that
#   and see whether JPL compiles correctly and passes all tests
#
# ===========================================================================
# This script passes https://www.shellcheck.net/
# ===========================================================================

# ===========================================================================
# TODO/BUGS
#
# - automatically get the latest hamcrest and junit jars (used by JPL, the
#   Java-Prolog bridge) from:
#
#   https://mvnrepository.com/artifact/org.hamcrest/hamcrest/ (currently 2.2; BSD 3 clause)
#   https://mvnrepository.com/artifact/junit/junit/           (currently 4.13.1; EPL 1.0)
#
# (On a longer timeframe, move JPL to Junit Jupiter)
#
# - Disentangle building manual PDF
#
# - Copying of modified files for rebuilding documentation is messy. Improve!
#
# - When building: Do not stop if /usr/local/logic does not exist. Instead,
#   just warn and proceed with building. One can always add it at 
#   ninja install time.
#
# - When building: Immediately check whether cmake and ninja exist and
#   exit if not. Same for the compiler
#
# - Need to install some stuff manually first. On Fedora with dnf:
#   dnf install cmake          #(make system)
#   dnf install ninja-build    #(build system)
#   dnf install gcc            #(C+ compiler)
#   dnf install gcc-c++        #(C++ compiler)
#   dnf install zlib-devel     #(compression)
#   dnf install openssl-devel  #(crypto)
#   dnf install readline-devel #(handling of command line)
#   dnf install libedit-devel  #(not sure whether both readline and libedit are needed)
#   dnf install pcre-devel     #(Perl regex support, optional)
#   dnf install libyaml-devel  #(YAML files support, optional)
#   dnf install uuid-devel     #(UUID support, optional - NOT libuuid-devel)
#   dnf install gmp-devel      #(GNU multiprecision library, long number support, recommended!)
#
# Not in the above:
#
# Google multi-threaded malloc                (tcmalloc)
# Berkeley Database Support                   (bdb)
# Java bridge support                         (jpl)
# ODBC support                                (odbc)
# Graphical interface support                 (pce)  - Needs X11, Qt5 (I think)
#
# - Properly exit if configuration fails!
# ===========================================================================

# set -x # Uncomment for tracing information
set -o nounset # No unset bash variables allowed

# ===========================================================================
# Obtain the URL of a remote repository to clone, as well as the
# name of a local directory to clone it into, plus other flags.
# Returns a composite: URL|TARGET_DIR_NAME|MODULES_YES_NO|INSTALL_DEST
# This is a "configuration procedure"
#
# An exit here doesn't exit the script because this is called in a subshell
# ===========================================================================

perso_github_account=https://github.com/dtonhofer        # Probably want to change that!!
swipl_github_account=https://github.com/SWI-Prolog       # Well-known and respected
system_install_dir=/usr/local/logic                      # This looks like a good place to me
toplevel_dir_fq="$HOME/Development/2021_02"              # Where to put stuff locally

giturl_and_subdir() {
   local finality=${1:-}                  # "jpl" or "docs" or "system"
   local element=${2:-}                   # "forked" (my personal fork) or "infra" (the original repo)
   local code="$finality,$element"
   case "$code" in
   jpl,forked)
      echo "$perso_github_account/packages-jpl.git | forked_jplmodule | modules_no | _ |"
      ;;
   jpl,infra)
      echo "$swipl_github_account/swipl-devel.git | master_swipldevel | modules_yes | locally |"
      ;;
   docs,forked)
      echo "$perso_github_account/swipl-devel.git | forked_swipldevel   | modules_no | _ |"
      ;;
   docs,infra)
      echo "$swipl_github_account/swipl-devel.git | master_swipldevel | modules_yes | locally |"
      ;;
   system,infra) # has no element
      echo "$swipl_github_account/swipl-devel.git | master_swipldevel | modules_yes | $system_install_dir |"
      ;;
   *)
      echo "In giturl_and_subdir(): Don't know how to handle code '$code' -- exiting!" >&2
      exit 1
   esac
}

# ===========================================================================
# Trim a string. This still doesn't exist in bash?
# ===========================================================================

trim() {
   echo "$1" | awk '{gsub(/^[ \t]+| [ \t]+$/,""); print $1 }'
}

# ===========================================================================
# Build a version string from the git log available for the *current
# directory*. This works if the current directory is under git management.
# The version sting is output as text, so capture this in the caller.
# Calls perl.
#
# An exit here doesn't exit the script because this is called in a subshell
#
# (this is currently unused)
# ===========================================================================

generate_version_string_from_git_log() {
   local gitline
   gitline=$(git log -n 1 --date=iso | grep 'Date:') || {
      echo "Could not obtain git log line in directory '$(pwd)'" >&2
      exit 1
   }
   local version
   version=$(echo "$gitline" | perl -e '$line = <>; if ($line =~ /\b(\d{4}-\d{2}-\d{2})\b/) { print "$1\n" }')
   if [[ -z "$version" ]]; then
      echo "Version/Date string from 'git log' is '$version' and not recognized" >&2
      exit 1
   else
      echo "$version"
   fi
}

# ===========================================================================
# Build a version string from the contents of the VERSION file in the
# current directory. The VERSION file must exist and its contents must be
# recognized, otherwise exit 1 is called.
# This works only if the current directory is the SWI-Prolog toplevel.
#
# An exit here doesn't exit the script because this is called in a subshell
# ===========================================================================

generate_version_string_from_version_file() {
   local versionfile="VERSION"
   local version
   version=$(<"$versionfile") || {
      echo "Could not read version file from '$versionfile' in directory '$(pwd)'" >&2
      exit 1
   }
   version=$(echo "$version" | grep --perl-regex '^\d+\.\d+\.\d+$')
   if [[ -z "$version" ]]; then
      echo "Version string from '$versionfile' not recognized" >&2
      exit 1
   else
      echo "$version"
   fi
}

# ===========================================================================
# Querying the user for a binary response. You have to hit enter after Y/N.
# Pipe "yes" into this script for auto-feeding.
#
# Call    : confirm "message"
# Returns : 0 for "YES" and 1 for "NO"
# Exits after 10 erroneous entries.
# ===========================================================================

confirm_with_user() {
   local msg="$1"
   local answer
   local maxtry=10
   while [ $maxtry -gt 0 ]; do
      echo "${msg} [Y/N]" # msg will probably include an "?"
      read -r answer
      answer=${answer^^} # upperase
      case "$answer" in
         YES|Y)  return 0
                 ;;
         NO|N)   return 1
                 ;;
         *)      ((maxtry--))
                 ;;
      esac
      echo "Please answer yes or no"
   done
   echo "ERROR: tried too many times to get YES/NO answer from user; aborting!" >&2
   exit 1
}

# ===========================================================================
# Supposing the current directory is the repository, look for a VERSION file
# ===========================================================================

look_for_version_file() {

   local version

   if [[ -f VERSION ]]; then
      version=$(generate_version_string_from_version_file) || {
         echo "Could not generate a version string from the version $(pwd)/VERSION -- exiting" >&2
         exit 1
      }
   else
      echo "There is no VERSION file in '$(pwd)' -- exiting" >&2
      exit 1
   fi

   global_version=$version

}

# ===========================================================================
# Clone from github
# ===========================================================================

clone() {

   local toplevel_dir_fq=${1:-}   # the directory we will be working in
   local finality=${2:-}          # "jpl" or "docs" or "system"
   local element=${3:-}           # "forked" or "infra" repo, possibly unset if not needed
   local here="clone"             # the name of this routine

   # The following exits in case of problems, otherwise cd-s to the directory
   # immediately above the repodir with an initial "pushd" (so we can "popd" later).
   # It also sets global variables with info about URL and subdir,

   dirchange_prepare "$here" "$toplevel_dir_fq" clone "$finality" "$element"

   local giturl="$global_giturl"
   local element_dir="$global_element_dir"
   local modules_yesno="$global_modules_yesno"

   if [[ -d "$element_dir" ]]; then
      echo "The directory with the cloned repo '$(pwd)/$element_dir' already exists -- exiting!" >&2
      echo "Maybe you just want to update the repo?" >&2
      exit 1
   fi

   # Re-confirm with user before going on

   echo "Going to clone the remote repo at : $giturl" >&2
   echo "Into this directory               : $(pwd)/$element_dir" >&2

   if ! confirm_with_user "Proceed?" ; then
      echo "Maybe later then" >&2
      exit 0
   fi

   # First clone into a temporary directory (because things may go wrong)

   local tmp_dir
   tmp_dir=$(mktemp -d -p .) || {
      echo "Could not create temporary directory in '$(pwd)' -- exiting!" >&2
      exit 1
   }

   pushd "$tmp_dir" >/dev/null || {
      echo "Could not cd to temporary directory '$tmp_dir' -- exiting!" >&2
      exit 1
   }

   echo "Cloning remote repo into temporary directory '$(pwd)/$element_dir' ..." >&2

   git clone "$giturl" "$element_dir" || {
      echo "Could not clone '$giturl' to directory '$element_dir' in '$(pwd)' -- exiting!" >&2
      exit 1
   }

   cd "$element_dir" || {
      echo "Could not cd to '$(pwd)/$element_dir' -- exiting!" >&2
      exit 1
   }

   if [[ $modules_yesno == modules_yes ]]; then
      echo "Cloning submodules in '$(pwd)' ..." >&2
      git submodule update --init || {
         echo "Error while cloning submodules in '$(pwd)' -- exiting!" >&2
         exit 1
      }
   fi

   popd >/dev/null || exit 1

   # We are now back in the finality-dependent directory.
   # Move the target directory to its correct location, then clean up

   mv "$tmp_dir/$element_dir" . || {
      echo "Could not move sub directory '$tmp_dir/$element_dir' to directory '$(pwd)' -- exiting!" >&2
      exit 1
   }

   echo "Successfully cloned into directory '$(pwd)/$element_dir'" >&2

   rmdir "$tmp_dir" || {
      echo "Could not remove temporary directory '$(pwd)/$tmp_dir' -- exiting!" >&2
      exit 1
   }

   # In case this is about documentation, create symlinks to certain files

   if [[ "${finality},${element}" == docs,forked ]]; then
      if [[ ! -e builtin.doc ]]; then
         ln -s "${element_dir}/man/builtin.doc" "builtin.doc"
         ln -s "${element_dir}/man/overview.doc" "overview.doc"
         # Add more here
      fi
   fi

   popd >/dev/null || exit 1

}

# ===========================================================================
# Copy local files prior to build
# ===========================================================================

copy() {

   local toplevel_dir_fq=${1:-}        # the directory we will be working in
   local finality=${2:-}               # "jpl" or "docs" or "system"
   local here="copy"

   # Exits in case of problems otherwise cds to the directory above the repodirs
   # with an initial push (so we can popd later)

   dirchange_prepare "$here" "$toplevel_dir_fq" copy "$finality"

   # Two directories are involed: the one with the SWI-Prolog distro that can be compiled
   # and the one with modified files. We need to copy modified files from the second to the first.

   local giturl_and_subdir_infra

   giturl_and_subdir_infra=$(giturl_and_subdir "$finality" infra) || {
      echo "Did not obtain valid values from giturl_and_subdir()  -- exiting" >&2
      exit 1
   }

   local giturl_and_subdir_forked

   giturl_and_subdir_forked=$(giturl_and_subdir "$finality" forked) || {
      echo "Did not obtain valid values from giturl_and_subdir()  -- exiting" >&2
      exit 1
   }

   local infra_dir
   local forked_dir

   infra_dir=$(echo "$giturl_and_subdir_infra" | cut --field=2 --delimiter='|')
   infra_dir=$(trim "$infra_dir")
   forked_dir=$(echo "$giturl_and_subdir_forked" | cut --field=2 --delimiter='|')
   forked_dir=$(trim "$forked_dir")

   local exit_now=

   if [[ ! -d "$infra_dir" ]]; then
      echo "The 'infrastructure' (target) directory '$(pwd)/$infra_dir' does not exist -- exiting" >&2
      exit_now=1
   fi

   if [[ ! -d "$forked_dir" ]]; then
      echo "The 'forked' (source) directory '$(pwd)/$forked_dir' does not exist -- exiting" >&2
      exit_now=1
   fi

   if [[ -n $exit_now ]]; then
      exit 1
   fi

   # Re-confirm with user before going on

   local origin_dir
   local target_dir
   origin_dir="$(pwd)/$forked_dir"
   target_dir="$(pwd)/$infra_dir"

   echo "Going to copy files from directory : '$origin_dir'" >&2
   echo "to directory                       : '$target_dir'" >&2

   declare -a from_files
   declare -a to_files

   # *******
   # Change this if your fileset changes
   # TODO: Make this a bit more flexible
   # *******

   case "$finality" in
      jpl)
         from_files=( "jpl.pl"              "test_jpl.pl" )
         to_files=(   "packages/jpl/jpl.pl" "packages/jpl/test_jpl.pl" )
      ;;
      docs)
         # Make this configurable
         # from_files=( "man/builtin.doc" "man/extensions.doc" "man/pl.bib" "man/runtex" "man/gen/swipl.bbl" "library/apply.pl" "src/Tests/library/test_apply.pl" )
         # to_files=(   "man/builtin.doc" "man/extensions.doc" "man/pl.bib" "man/runtex" "man/gen/swipl.bbl" "library/apply.pl" "src/Tests/library/test_apply.pl" )
         from_files=( "man/builtin.doc" "man/extensions.doc" "man/overview.doc" )
         to_files=(   "man/builtin.doc" "man/extensions.doc" "man/overview.doc" )
      ;;
      *)
         echo "Unknown finality '$finality'" >&2
         exit 1
   esac

   local i=0

   while [[ $i < ${#from_files[@]} ]]; do
      local from_file="${from_files[$i]}"
      local to_file="${to_files[$i]}"
      echo "Would copy '${from_file}' --> '${to_file}" >&2
      ((i++))
   done

   if ! confirm_with_user "Proceed?" ; then
      echo "Maybe later then" >&2
      exit 0
   fi

   i=0
   while [[ $i < ${#from_files[@]} ]]; do
      local from_file_fq="${origin_dir}/${from_files[$i]}"
      local to_file_fq="${target_dir}/${to_files[$i]}"
      echo "Copying '${from_file_fq}' --> '${to_file_fq}" >&2
      /bin/cp "${from_file_fq}" "${to_file_fq}" || {
         echo "Could not copy -- exiting" >&2
         exit 1
      }
     ((i++))
   done

   popd >/dev/null || exit 1

}

# ===========================================================================
# Checking what is checked out
# ===========================================================================

look() {

   local toplevel_dir_fq=${1:-}  # the directory we will be working in
   local finality=${2:-}         # "jpl" or "docs" or "system"
   local element=${3:-}          # "infra" or "forked" or unset
   local here="look"             # The name of this routine

   # The following exits in case of problems, otherwise cd-s to the directory
   # immediately above the repodir with an initial "pushd" (so we can "popd" later).
   # It also sets global variables with info about URL and subdir,

   dirchange_prepare "$here" "$toplevel_dir_fq" look "$finality" "$element"

   echo "Currently in this directory: $(pwd)"
   echo

   # the above performed "pushd" to "work_dir_fq" and then
   # "cd" to "infra_dir": we are in the repository directory

   echo "---"
   echo "Latest git log entry"
   echo "---"

   git log -n 1 --date=iso

   echo "---"
   echo "Any differences between the remote master and this master?"
   echo "---"

   git diff remotes/origin/master..master

   echo "---"
   echo "The VERSION file says"
   echo "---"

   look_for_version_file
   echo "$global_version"

   echo "---"
   echo "git status says"
   echo "---"

   git status

   echo "---"
   echo "git remote show origin"
   echo "---"

   git remote show origin

   popd >/dev/null || exit 1

}

# ===========================================================================
# Build or rebuild an SWI-Prolog distribution that has already been
# downloaded from github.
# ===========================================================================

build() {

   local toplevel_dir_fq=${1:-}  # the directory we will be working in
   local finality=${2:-}         # "jpl" or "docs" or "system"
   local rebuild=${3:-}          # if set to "rebuild", then just rebuild (if possible), otherwise build
   local arg1=${4:-}             # may be "withpdf" or "notest" or "mono"
   local arg2=${5:-}             # idem
   local arg3=${6:-}             # idem
   local here="build"            # the name of this routine

   local withpdf=
   if [[ $arg1 == withpdf || $arg2 == withpdf || $arg3 == withpdf ]]; then
      withpdf=yes
   fi

   local notest=
   if [[ $arg1 == notest || $arg2 == notest || $arg3 == notest ]]; then
      notest=yes
   fi

   local mono=
   if [[ $arg1 == mono || $arg2 == mono || $arg3 == mono ]]; then
      mono=yes
   fi

   if [[ $rebuild == rebuild && $withpdf == yes ]]; then
      echo "Just a 'rebuild' demanded but 'withpdf' needs 'build' -- exiting!" >&2
      exit 1
   fi

   # The following exits in case of problems, otherwise cd-s to the directory
   # immediately above the repodir with an initial "pushd" (so we can "popd" later).
   # It also sets global variables with info about URL and subdir,

   dirchange_prepare "$here" "$toplevel_dir_fq" build "$finality"

   local install_location="$global_install_location"

   look_for_version_file
   local version="$global_version"

   # Where to install? It depends!

   if [[ $install_location == locally ]]; then
      # local directory based on version string
      # (should we also base it on the git log hash? maybe as an option?)
      install_dir_fq="$global_work_dir_fq/swiplexe_${version}"
   else
      if [[ ! -d $global_install_location ]]; then
         echo "The installation location '$global_install_location' does not exist -- exiting!" >&2
         exit 1
      fi
      install_dir_fq="$global_install_location/swiplexe_${version}"
   fi

   # As we are currently in the distro directory, configure & compile in here!

   local build_dir="build"
   local build_dir_fq="$(pwd)/build"  # for later, to find it back easily

   if [[ -d $build_dir ]]; then
      if [[ $rebuild != rebuild ]]; then
         echo "Full build ordered but directory '$build_dir' already exists in '$(pwd)' -- removing it" >&2
         /bin/rm -rf "$build_dir" || {
            echo "Problems removing '$build_dir' in '$(pwd)' -- exiting!" >&2
            exit 1
         }
      fi
   else
      if [[ $rebuild == rebuild ]]; then
         echo "Rebuild ordered but build directory does not exist -- performing a full build" >&2
         rebuild=build
      fi
   fi

   if [[ ! -d $build_dir ]]; then
      mkdir "$build_dir" # No need to check whether it worked; we will try to cd to it in any case
   fi

   cd "$build_dir" || {
      echo "Could not cd to '$build_dir' in '$(pwd)' -- exiting!" >&2
      exit 1
   }

   # jars may or may not exist; compilation works without them; but if they are there, then
   # the Java bridge "jpl" can be tested

   local jar_dir_fq="${toplevel_dir_fq}/jars"
   local hamcrest_jar_fq="${jar_dir_fq}/hamcrest-2.2.jar"  # the version number has to updated sometimes
   local junit_jar_fq="${jar_dir_fq}/junit-4.13.1.jar"     # the version number has to updated sometimes

   # Re-confirm with user (actually a bit late as we have already removed the build dir)

   echo "Going to build the SWI-Prolog distro in: $(pwd)"            >&2
   echo "The installation directory is          : $install_dir_fq"   >&2
   if [[ $rebuild == rebuild ]]; then
   echo "Not performing a full build, just a rebuild" >&2
   else
   echo "Performing a full build" >&2
   if [[ $withpdf == yes ]]; then
   echo "Also building PDF documentation" >&2
   fi
   fi
   if [[ -d "$jar_dir_fq" ]]; then
   echo "The following jar directory exists     : $jar_dir_fq"      >&2
   if [[ -f "$hamcrest_jar_fq" ]]; then
   echo "   and the hamcrest jar exists         : $hamcrest_jar_fq" >&2
   else
   echo "   But hamcrest jar IS MISSING         : $hamcrest_jar_fq" >&2
   echo "   get it from: https://mvnrepository.com/artifact/org.hamcrest/hamcrest/" >&2
   fi
   if [[ -f "$junit_jar_fq" ]]; then
   echo "   and the junit4 jar exists           : $junit_jar_fq"    >&2
   else
   echo "   But junit4 jar IS MISSING           : $junit_jar_fq"    >&2
   echo "   get it from: https://mvnrepository.com/artifact/junit/junit/" >&2
   fi
   else
   echo "ATTENTION! There is no jar directory   : $jar_dir_fq"      >&2
   fi

   if ! confirm_with_user "Proceed?" ; then
      exit 0
   fi

   # configure by calling cmake if this is NOT a "rebuild"

   if [[ $rebuild != rebuild ]]; then
      local hamcrest_line=
      local junit_jar_line=
      local pdf_line=
      if [[ -d "$jar_dir_fq" ]]; then
         echo "Building with jars in '$jar_dir_fq'" >&2
         hamcrest_line="-DHAMCREST=$hamcrest_jar_fq"
         junit_jar_line="-DJUNIT_JAR=$junit_jar_fq"
      else
         echo "Building without jars as '$jar_dir_fq' does not exist" >&2
      fi
      if [[ $withpdf == yes ]]; then
         pdf_line="-DBUILD_PDF_DOCUMENTATION=ON"
         #
         # This should result in a PDF file: build/man/SWI-Prolog-8.3.16.pdf
         # This is very dicey!! Building PDF from TeX doc and Bibliography may fail for various
         # very obscure reasons (in particular missing tools and an unclear processing chain).
         # How to make this reliable?
         # There won't be any errors in the log either, cmake seems to suppress them
         # On failure, try to build the documentation manually but be prepared for suffering.
         # The first step in debugging is running the script manually:
         # It's called "runtex" and it has to be run from build/man:
         # cd build/man; ../../man/runtex --pdf SWI-Prolog-8.3.16.tex
         #
         # Extra: pdflatex is not "unicode aware"; any "ASCII images" cannot contain unicode. How to fix?
         #
      fi
      # cmake can handle "empty arguments" so no special handling of "" - excellent!
      cmake \
         "-DCMAKE_INSTALL_PREFIX=$install_dir_fq" \
         "$hamcrest_line" \
         "$junit_jar_line" \
         "$pdf_line" \
         "" \
         "-DLIBEDIT_LIBRARIES=/usr/lib64/libedit.so" \
         "-DLIBEDIT_INCLUDE_DIR=/usr/include/editline" \
         -G Ninja ..
   fi

   # compile

   ninja || {
      echo "'ninja' command failed -- exiting" >&2
      exit 1
   }

   # Test

   if [[ $notest != yes ]]; then
      run_post_build_tests $mono
   fi

   # Run some standard checks from SWIPL (even though it has not been installed yet)
   # See https://eu.swi-prolog.org/pldoc/doc/_SWI_/library/check_installation.pl

   echo "Checking for installation problems using Prolog goal 'check_installation.'" >&2

   ./src/swipl -g "check_installation,halt."

   echo
   echo "Note that:"
   echo "1) A missing tcmalloc is not necessarily a problem. SWI-Prolog works"
   echo "   perfectly well without it."
   echo "   http://www.swi-prolog.org/build/issues/tcmalloc.html"
   echo
   echo "2) A missing library(ODBC) is not a problem if you are not going to"
   echo "   access relational databases from Prolog."
   echo "   http://www.swi-prolog.org/build/issues/odbc.html"
   echo
   echo "3) A missing library(yaml) is not a problem if you are not going to"
   echo "   handle YAML text files."
   echo "   https://en.wikipedia.org/wiki/YAML"
   echo "   http://www.swi-prolog.org/build/issues/yaml.html"
   echo

   # Maybe install
   # (What happens if the installation directory exists? Is it replaced?)

   if [[ $finality != system ]]; then
      ninja install
   fi

   # retain the build directory for the caller

   global_build_dir_fq=$(pwd)
   global_install_dir_fq="$install_dir_fq"

   local cmake_logfile
   local file_length

   for cmake_logfile in "CMakeFiles/CMakeOutput.log" "CMakeFiles/CMakeError.log"; do
      if [[ -f "$cmake_logfile" ]]; then
         echo -n "You may want to inspect CMake logfile '$(pwd)/$cmake_logfile' ... "
         file_length=$(wc -l "$cmake_logfile" | sed 's/\s.*$//g')
         echo "it has $file_length lines"
      else
         echo "There is no CMake logfile '$(pwd)/$cmake_logfile' ... weird!"
      fi
   done
 
   # back to the directory immediately above the repodir

   popd >/dev/null || exit 1

   # if this is about documentation, create certain symlinks (TODO: move out to a separate command)

   if [[ "${finality}" == docs ]]; then
      create_symlinks_to_built_docs "$(pwd)" "$install_dir_fq" "$build_dir_fq" "$version"
   fi

}

# ===========================================================================
# Set up symlinks to built documentation (in the install dir, except for
# the manual's PDF, which is in the build dir)
# ===========================================================================

create_symlinks_to_built_docs() {
   local where=${1}           # the directory above the repodir
   local install_dir_fq=${2}  # the fully qualified install dir (TODO: relativize relative to "where")
   local build_dir_fq=${3}    # the fully qualified build dir (TODO: relativize relative to "where")
   local version=${4}         # the version string; needed to find the manual's PDF
   pushd "$where" || {
      echo "Could not cd to '$where' -- exiting" >&2 
      exit 1
   }
   if [[ -s builtin.html ]]; then
      /bin/rm builtin.html
   fi
   if [[ -s overview.html ]]; then
      /bin/rm overview.html
   fi
   if [[ -s manual.pdf ]]; then
      /bin/rm manual.pdf
   fi
   create_symlink_or_skip "${install_dir_fq}/lib/swipl/doc/Manual/overview.html" "overview.html"
   create_symlink_or_skip "${install_dir_fq}/lib/swipl/doc/Manual/builtin.html"  "builtin.html"
   # manual PDF only exists if PDF has been requested
   create_symlink_or_skip "${build_dir_fq}/man/SWI-Prolog-${version}.pdf"        "manual.pdf"
}

create_symlink_or_skip() {
   local target=${1}
   local name=${2}
   if [[ -f $target ]]; then
      ln -s "$target" "$name" || {
         echo "Could not create symlink to '$target' in '$(pwd)'" >&2
      }
   else
      echo "File '$target' does not exist -- skipping symlink creation in '$(pwd)'" >&2
   fi
}

# ===========================================================================
# Run post-build tests. This is called when in the correct directory!
# ===========================================================================

run_post_build_tests() {

   local mono=${1:-}

   # Delete the logfile prior to tests.

   local logfile="Testing/Temporary/LastTest.log"

   if [[ -f "$logfile" ]]; then
      echo "-------" >&2
      echo "There already is a logfile '$logfile' -- deleting it" >&2
      echo "-------" >&2
      /bin/rm "$logfile"
   fi

   # Run tests concurrently 4-fold. See "man ctest" or "ctest --help"

   local jobs

   if [[ $mono == yes ]]; then
      jobs=1
   else
      jobs=4
   fi

   ctest -j $jobs || {
      echo "The test failed!" >&2
      echo "More info in directory $(pwd)/Testing/Temporary/" >&2
      tree "Testing/Temporary" >&2
      echo "Check file '$(pwd)/$logfile'" >&2
      exit 1
   }

   # Always grep for ERROR even in case of success. There may also be warnings, but
   # they are probably of low interest.

   if [[ -f "$logfile" ]]; then
      local errfile
      errfile=$(mktemp) || {
         echo "Could not create a temporary file to capture error messages -- exiting" >&2
         exit 1
      }
      grep ERROR "$logfile" >> "$errfile"
      grep WARN "$logfile"  >> "$errfile"
      local size
      size=$(stat --format=%s "$errfile")
      if [[ $size -gt 0 ]]; then
         echo "Some errors were found in the logfile '$(pwd)/$logfile' (probably not a problem)" >&2
         echo >&2
         echo "--------------" >&2
         cat "$errfile" >&2
         echo "--------------" >&2
         echo >&2
      fi
      /bin/rm "$errfile"
   fi

}

# ===
# Move to the correct directory, possibly creating it.
# ===

dirchange_prepare() {

   local caller=${1:-}            # Name of the caller, for messages
   local toplevel_dir_fq=${2:-}   # The directory containing the finality-named subdirectories containing the repo directories
   local action_full=${3:-}       # What do you want to do? "clone", "copy", "build"/"rebuild", "look"
   local finality=${4:-}          # What's it for? "docs", "jpl", "system"
   local element=${5:-}           # What repository to deal with? "forked", "infra". May also be unset

   if [[ -z "$toplevel_dir_fq" || ! -d "$toplevel_dir_fq" ]]; then
      echo "You must pass an existing toplevel directory to '$caller'." >&2
      echo "Apparently '$toplevel_dir_fq' either doesn't exist or is not a directory -- exiting!" >&2
      exit 1
   fi

   # Reduce a "rebuild" action to a "build" action

   local action
   if [[ $action_full == rebuild ]]; then
      action=build
   else
      action=$action_full
   fi

   # If we want to clone/build/look if finality is "system", it's always about
   # the infrastructure; no need for "element".

   if [[ $finality == system ]]; then
     element=
   fi

   # Lookup control flags

   local code="$action,$finality,$element"
   local can_create=

   case $code in
      clone,system,)
         can_create=true
         ;;
      clone,jpl,forked)
         can_create=true
         ;;
      clone,jpl,infra)
         can_create=true
         ;;
      clone,docs,forked)
         can_create=true
         ;;
      clone,docs,infra)
         can_create=true
         ;;
      look,system,)
         ;;
      look,jpl,forked)
         ;;
      look,jpl,infra)
         ;;
      look,docs,forked)
         ;;
      look,docs,infra)
         ;;
      build,system,)
         ;;
      build,jpl,)
         ;;
      build,docs,)
         ;;
      copy,jpl,)
         ;;
      copy,docs,)
         ;;
      *)
         # TODO: This error message is extremely confusing. The only way out is to display --help...
         # "Unknown action,finality,element code 'clone,,forked' in 'clone' -- exiting!"
         echo "Unknown action,finality,element code '$action_full,$finality,$element' in '$caller' -- exiting!" >&2
         exit 1
   esac

   # Maybe create finality-dependent subdir under $toplevel_dir_fq (which exists)

   local work_dir_fq="$toplevel_dir_fq/$finality"
   global_work_dir_fq="$work_dir_fq"

   if [[ ! -d $work_dir_fq && -n $can_create ]]; then
      echo "Work directory '$work_dir_fq' does not exist -- creating it!" >&2
      mkdir -p "$work_dir_fq" # Make with parent; no need to check whether it worked; we will try to cd to it in any case
   fi

   # cd to finality-dependent subdir but use "pushd" so that a later "popd" will get us back!

   pushd "$work_dir_fq" >/dev/null || {
      echo "Could not cd to '$work_dir_fq' (that directory should exist at this point) -- exiting!" >&2
      exit 1
   }

   # Next level down are the directories which are the root directories of the repositories.

   # In case of "copy" we interested in two of them. We can stop here, the caller will take over.

   if [[ $action == copy ]]; then
      return
   fi

   # In case of "build" we always go to the "infra" element (not the forked one)

   if [[ $action == build || $finality == system ]]; then
      element=infra
   fi

   # Get more info from the table holding URL and subdirectory names

   local giturl_and_subdir
   giturl_and_subdir=$(giturl_and_subdir "$finality" "$element") || {
      echo "Did not obtain valid value from giturl_and_subdir() -- exiting!" >&2
      exit 1
   }

   # This one is always needed

   global_element_dir=$(echo "$giturl_and_subdir" | cut --field=2 --delimiter='|')
   global_element_dir=$(trim "$global_element_dir")

   # In case of "clone" just get some more info, then return

   if [[ $action == clone ]]; then
      global_giturl=$(echo "$giturl_and_subdir" | cut --field=1 --delimiter='|')
      global_giturl=$(trim "$global_giturl")
      global_modules_yesno=$(echo "$giturl_and_subdir" | cut --field=3 --delimiter='|' )
      global_modules_yesno=$(trim "$global_modules_yesno")
   fi

   # In case of "build/rebuild/look" verify that the directory exist, get some more info, then return

   if [[ $action == build || $action == look ]]; then
      if [[ ! -d "$global_element_dir" ]]; then
         echo "The 'element' directory '$(pwd)/$global_element_dir' does not exist -- exiting!" >&2
         exit 1
      fi
      cd "$global_element_dir" || {
         echo "Could not cd to '$(pwd)/$global_element_dir' -- exiting" >&2
         exit 1
      }
      global_install_location=$(echo "$giturl_and_subdir" | cut --field=4 --delimiter='|')
      global_install_location=$(trim "$global_install_location")
   fi

}

# ===========================================================================
# Command dispatch
# ===========================================================================

cmd_match() {
   local arg=${1:-}
   if [[ -z $cmd ]]; then
      if [[ $arg == clone   ||
            $arg == copy    ||
            $arg == build   ||
            $arg == rebuild ||
            $arg == look ]]; then
         cmd=$arg  # set global variable
      fi
   fi
}

finality_match() {
   local arg=${1:-}
   if [[ -z $finality ]]; then
      if [[ $arg == system ||
            $arg == jpl    ||
            $arg == docs ]]; then
         finality=$arg  # set global variable
      fi
   fi
}

cmd=
finality=
arg3=${3:-}
arg4=${4:-}
arg5=${5:-}

# this is done in order to make the argument order irrelevant:

cmd_match      "${1:-}"   # maybe sets variable cmd, which must be still unset
finality_match "${1:-}"   # maybe sets variable finality, which must still be unset
cmd_match      "${2:-}"   # maybe sets variable cmd, which must still be unset
finality_match "${2:-}"   # maybe sets variable finality, which must still be unset

if [[ $cmd == clone ]]; then
   clone "$toplevel_dir_fq" "$finality" "$arg3"
   exit 0
fi

if [[ $cmd == copy ]]; then
   copy "$toplevel_dir_fq" "$finality"
   exit 0
fi

if [[ $cmd == look ]]; then
   look "$toplevel_dir_fq" "$finality" "$arg3"
   exit 0
fi

if [[ $cmd == build ||
      $cmd == rebuild ]]; then
   build "$toplevel_dir_fq" "$finality" "$cmd" "$arg3" "$arg4" "$arg5"
   if [[ $finality == system ]]; then
      echo "You have to run 'ninja install' as root in '$global_build_dir_fq' to install the compilate into '$global_install_dir_fq'" >&2
   fi
   exit 0
fi

if [[ -n "$cmd" ]]; then
   echo "Command '$cmd' was not recognized" >&2
fi

cat <<TEXT
Expecting the following. The first two arguments can be provided in any order.

For cloning remote repo

    clone system         : Clone original SWI Prolog repo, including submodules, to build for a systemwide distribution
    clone jpl     forked : Clone modified JPL package from the personal github account, for editing JPL code
    clone jpl     infra  : Clone original SWI Prolog repo, including submodules, to build for JPL testing
    clone docs    forked : Clone modified SWI Prolog repo from the personal github account, for editing documentation
    clone docs    infra  : Clone original SWI Prolog repo, including submodules, to build documentation

For preparing to build

    copy jpl             : Copy specific files from the JPL forked & modified repodir into the JPL build dir
    copy docs            : Copy specific files from the documentation forked & modified repodir into the docs build dir

For building/rebuilding

    build system         : Build a systemwide installation (will go to $system_install_dir)
    build docs           : Build local installation to check modified SWI-Prolog documentation
    build jpl            : Build local installation to check modified JPL code

   with additional optional flags on positions 3 or 4:

    withpdf              : Additionally build the PDF documentation
    notest               : Skip post-build tests
    mono                 : No parallel tests

For getting information about the status of the currently checked-out files.

    look system          : What's check out for the systemwide installation?
    look jpl      forked : What's checked out in the JPL forked repo?
    look jpl      infra  : What's checked out in the JPL build repo?
    look docs     forked : What's checked out in the docs forked repo?
    look docs     infra  : What's checked out in the docs build repo?

Hints about manual commands:

    To branch : Run something like "git checkout -b docs_202012"

TEXT

exit 1

