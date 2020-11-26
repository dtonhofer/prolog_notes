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

# ---
# TODO: automatically get the latest hamcrest and junit jars (used by JPL)
# from:
#
#   https://mvnrepository.com/artifact/org.hamcrest/hamcrest/ (currently 2.2; BSD 3 clause)
#   https://mvnrepository.com/artifact/junit/junit/           (currently 4.13.1; EPL 1.0)
#
# (On a longer timeframe, move JPL to Junit Jupiter)
# ---

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
toplevel_dir_fq="$HOME/Development/2020_11/swiplmaking"  # Where to put stuff locally

giturl_and_subdir() {
   local finality=${1:-}                  # "jpl" or "docu" or "system"
   local element=${2:-}                   # "forked" (my personal fork) or "infra" (the original repo)
   local code="$finality,$element"
   case "$code" in
   jpl,forked)
      echo "$perso_github_account/packages-jpl.git | packages-jpl_forked | modules_no | _ |"
      ;;
   jpl,infra)
      echo "$swipl_github_account/swipl-devel.git | swipl-devel_original | modules_yes | locally |"
      ;;
   docu,forked)
      echo "$perso_github_account/swipl-devel.git | swipl-devel_forked   | modules_no | _ |"
      ;;
   docu,infra)
      echo "$swipl_github_account/swipl-devel.git | swipl-devel_original | modules_yes | locally |"
      ;;
   system,infra) # has no element
      echo "$swipl_github_account/swipl-devel.git | swipl-devel_original | modules_yes | $system_install_dir |"
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
   local finality=${2:-}          # "jpl" or "docu" or "system"
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

   # In case this is about documentation, create a symlink to the file describing builtins

   if [[ "${finality},${element}" == docu,forked ]]; then
      if [[ ! -e builtin.doc ]]; then
         ln -s "${element_dir}/man/builtin.doc" "builtin.doc"
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
   local finality=${2:-}               # "jpl" or "docu" or "system"
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

   local origin_dir="$(pwd)/$forked_dir"
   local target_dir="$(pwd)/$infra_dir"

   echo "Going to copy files from directory : '$origin_dir'" >&2
   echo "to directory                       : '$target_dir'" >&2

   declare -a from_files
   declare -a to_files

   # *******
   # Change this if your fileset changes
   # *******

   case "$finality" in
      jpl)
         from_files=( "jpl.pl"              "test_jpl.pl" )
         to_files=(   "packages/jpl/jpl.pl" "packages/jpl/test_jpl.pl" )
      ;;
      docu)
         from_files=( "man/builtin.doc" "man/extensions.doc" )
         to_files=(   "man/builtin.doc" "man/extensions.doc" )
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
   local finality=${2:-}         # "jpl" or "docu" or "system"
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
   local finality=${2:-}         # "jpl" or "docu" or "system"
   local rebuild=${3:-}          # if set to "rebuild", then just "rebuild" if possible
   local here="build"            # the name of this routine

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
         echo "The installation location '$global_install_location' does not exist" >&2
         exit 1
      fi
      install_dir_fq="$global_install_location/swiplexe_${version}"
   fi

   # As we are currently in the distro directory, configure & compile in here!

   local build_dir="build"

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
         rebuild=
      fi
   fi

   if [[ ! -d $build_dir ]]; then
      mkdir "$build_dir" # No need to check whether it worked; we will try to cd to it in any case
   fi

   cd "$build_dir" || {
      echo "Could not cd to '$build_dir' in '$(pwd)' -- exiting" >&2
      exit 1
   }

   # jars may or may not exist; compilation works without them; but if they are there, then
   # the Java bridge "jpl" can be tested

   local jar_dir_fq="${toplevel_dir_fq}/jars"

   local hamcrest_jar_fq="${jar_dir_fq}/hamcrest-2.2.jar"
   local junit_jar_fq="${jar_dir_fq}/junit-4.13.1.jar"

   # Re-confirm with user (actually a bit late as we have already removed the build dir)

   echo "Going to build the SWI-Prolog distro in: $(pwd)"            >&2
   echo "The installation directory is          : $install_dir_fq"   >&2
   if [[ $rebuild == rebuild ]]; then
   echo "Not performing a full build, just a rebuild" >&2
   else
   echo "Performing a full build" >&2
   fi
   if [[ -d "$jar_dir_fq" ]]; then
   echo "The following jar directory exists     : $jar_dir_fq"      >&2
   if [[ -f "$hamcrest_jar_fq" ]]; then
   echo "   and the hamcrest jar exists         : $hamcrest_jar_fq" >&2
   else
   echo "   but there is no hamcrest jar        : $hamcrest_jar_fq" >&2
   echo "   get it from: https://mvnrepository.com/artifact/org.hamcrest/hamcrest/" >&2
   fi
   if [[ -f "$junit_jar_fq" ]]; then
   echo "   and the junit4 jar exists           : $junit_jar_fq"    >&2
   else
   echo "   but there is no junit4 jar          : $junit_jar_fq"    >&2
   echo "   get it from: https://mvnrepository.com/artifact/junit/junit/" >&2
   fi
   else
   echo "ATTENTION! There is no jar directory   : $jar_dir_fq"      >&2
   fi

   if ! confirm_with_user "Proceed?" ; then
      exit 0
   fi

   # maybe configure

   if [[ $rebuild != rebuild ]]; then
      if [[ -d "$jar_dir_fq" ]]; then
         echo "Building with jars in '$jar_dir_fq'" >&2
         cmake \
            -DCMAKE_INSTALL_PREFIX="$install_dir_fq" \
            -DHAMCREST="$hamcrest_jar_fq" \
            -DJUNIT_JAR="$junit_jar_fq" \
            -DLIBEDIT_LIBRARIES=/usr/lib64/libedit.so \
            -DLIBEDIT_INCLUDE_DIR=/usr/include/editline \
            -G Ninja ..
      else
         echo "Building without jars as '$jar_dir_fq' does not exist" >&2
         cmake \
            -DCMAKE_INSTALL_PREFIX="$install_dir_fq" \
            -DLIBEDIT_LIBRARIES=/usr/lib64/libedit.so \
            -DLIBEDIT_INCLUDE_DIR=/usr/include/editline \
            -G Ninja ..
      fi
   fi

   # compile

   ninja

   # Test

   # Delete the logfile prior to tests.

   local logfile="Testing/Temporary/LastTest.log"

   if [[ -f "$logfile" ]]; then
      echo "-------" >&2
      echo "There already is a logfile '$logfile' -- deleting it" >&2
      echo "-------" >&2
      /bin/rm "$logfile"
   fi

   # Run tests concurrently 4-fold. See "man ctest" or "ctest --help"

   ctest -j 4 || {
      echo "The test failed!" >&2
      echo "More info in directory $(pwd)/Testing/Temporary/" >&2
      tree "Testing/Temporary" >&2
      echo "Check file '$(pwd)/${logfile}'" >&2
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
         echo "Some errors were found in the logfile '$(pwd)/${logfile}' (probably not a problem)" >&2
         echo >&2
         echo "--------------" >&2
         cat "$errfile" >&2
         echo "--------------" >&2
         echo >&2
      fi
      /bin/rm "$errfile"
   fi

   # Run some standard checks from SWIPL (even though it has not been installed yet)
   # See https://eu.swi-prolog.org/pldoc/doc/_SWI_/library/check_installation.pl

   echo "Checking for installation problems using Prolog goal 'check_installation.'" >&2

   ./src/swipl -g "check_installation,halt."
 
   # Maybe install
   # (What happens if the installation directory exists? Is it replaced?)

   if [[ $finality != system ]]; then
      ninja install
   fi

   # retain the build directory

   global_build_dir_fq=$(pwd)
   global_install_dir_fq="$install_dir_fq"

   popd >/dev/null || exit 1

}

# ===
# Move to the correct directory, possibly creating it.
# ===

dirchange_prepare() {

   local caller=${1:-}            # Name of the caller, for messages
   local toplevel_dir_fq=${2:-}   # The directory containing the finality-named subdirectories containing the repo directories
   local action_full=${3:-}       # What do you want to do? "clone", "copy", "build"/"rebuild", "look"
   local finality=${4:-}          # What's it for? "docu", "jpl", "system"
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
   local must_exist_deeper=

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
      clone,docu,forked)
         can_create=true
         ;;
      clone,docu,infra)
         can_create=true
         ;;
      look,system,)
         ;;
      look,jpl,forked)
         ;;
      look,jpl,infra)
         ;;
      look,docu,forked)
         ;;
      look,docu,infra)
         ;;
      build,system,)
         ;;
      build,jpl,)
         ;;
      build,docu,)
         ;;
      copy,jpl,)
         ;;
      copy,docu,)
         ;;
      *)
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


cmd=${1:-}        # Command is first argument to script (clone, copy, install)
finality=${2:-}   # Finality is second argument to script (system, jpl, docu)
xarg=${3:-}       # There may be an extra argument
                  # For "clone" command: "forked" or "infra"

if [[ $cmd == clone ]]; then
   clone "$toplevel_dir_fq" "$finality" "$xarg"
   exit 0
fi

if [[ $cmd == copy ]]; then
   copy "$toplevel_dir_fq" "$finality"
   exit 0
fi

if [[ $cmd == look ]]; then
   look "$toplevel_dir_fq" "$finality" "$xarg"
   exit 0
fi

if [[ $cmd == build ]]; then
   build "$toplevel_dir_fq" "$finality" xxxxxxx
   if [[ $finality == system ]]; then
      echo "You have to run 'ninja install' as root in '$global_build_dir_fq' to install the compilate into '$global_install_dir_fq'" >&2
   fi
   exit 0
fi

if [[ $cmd == rebuild ]]; then
   build "$toplevel_dir_fq" "$finality" rebuild
   if [[ $finality == system ]]; then
      echo "You have to run 'ninja install' as root in '$global_build_dir_fq' to install the compilate into '$global_install_dir_fq'" >&2
   fi
   exit 0
fi

if [[ -n "$cmd" ]]; then
   echo "No command '$cmd'" >&2
fi

cat <<TEXT
Expecting:

For cloning remote repo

    clone system      : Clone original SWI Prolog repo, including submodules, to build for a systemwide distribution
    clone jpl forked  : Clone modified JPL package from the personal github account, for editing JPL code
    clone jpl infra   : Clone original SWI Prolog repo, including submodules, to build for JPL testing
    clone docu forked : Clone modified SWI Prolog repo from the personal github account, for editing documentation
    clone docu infra  : Clone original SWI Prolog repo, including submodules, to build documentation

For preparing to build

    copy jpl          : Copy specific files from the JPL forked & modified repodir into the JPL build dir
    copy docu         : Copy specific files from the documentation forked & modified repodir into the docu build dir

For building/rebuilding

    build system      : Build a systemwide installation (will go to $system_install_dir)
    build docu        : Build local installation to check modified SWI-Prolog documentation
    build jpl         : Build local installation to check modified JPL code

For getting information about the status of the currently checked-out files.

    look system       : What's check out for the systemwide installation?
    look jpl forked   : What's checked out in the JPL forked repo?
    look jpl infra    : What's checked out in the JPL build repo?
    look docu forked  : What's checked out in the docu forked repo?
    look docu infra   : What's checked out in the docu build repo?

Hints about manual commands:

    To branch : Run something like "git checkout -b docu_202007"

TEXT

exit 1

