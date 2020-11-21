#!/bin/bash

# TODO: automatically get the latest hamcrest and junit jars from
#
#   https://mvnrepository.com/artifact/org.hamcrest/hamcrest/
#   https://mvnrepository.com/artifact/junit/junit/

# set -x # Uncomment for tracing information

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

set -o nounset

# ===========================================================================
# Obtain the URL of a remote repository to clone, as well as the
# name of a local directory to clone it into, plus other flags.
# Returns a composite: URL|TARGET_DIR_NAME|MODULES_YES_NO|INSTALL_DEST
# This is a "configuration procedure"
#
# An exit here doesn't exit the script because this is called in a subshell
# ===========================================================================

perso_github_account=https://github.com/dtonhofer
swipl_github_account=https://github.com/SWI-Prolog
system_install_dir=/usr/local/logic
toplevel_dir_fq="$HOME/Development/2020_11/swiplmaking3"  # where to put stuff locally

url_and_dir() {
   local finality=${1:-}                  # "jpl" or "docu" or "system"
   local which_repo=${2:-}                # "forked" or "infra"
   local key="${finality}.${which_repo}"
   case "$key" in
   jpl.forked)
      echo "${perso_github_account}/packages-jpl.git | packages-jpl_forked | modules_no | _ |"
      ;;
   jpl.infra)
      echo "${swipl_github_account}/swipl-devel.git | swipl-devel_original | modules_yes | locally |"
      ;;
   docu.forked)
      echo "${perso_github_account}/swipl-devel.git | swipl-devel_forked   | modules_no | _ |"
      ;;
   docu.infra)
      echo "${swipl_github_account}/swipl-devel.git | swipl-devel_original | modules_yes | locally |"
      ;;
   system.infra)
      echo "${swipl_github_account}/swipl-devel.git | swipl-devel_original | modules_yes | ${system_install_dir} |"
      ;;
   *)
      echo "In url_and_dir(): Don't know how to handle key '$key'" >&2
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
# Clone from github
# ===========================================================================

clone() {

   local toplevel_dir_fq=${1:-}   # the directory we will be working in
   local finality=${2:-}          # "jpl" or "docu" or "system"
   local which_repo=${3:-}        # "forked" or "infra" repo

   if [[ -z "$toplevel_dir_fq" || ! -d "$toplevel_dir_fq" ]]; then
      echo "You must pass an existing (fully qualified) toplevel directory to subroutine clone()" >&2
      echo "but it seems '$toplevel_dir_fq' either doesn't exist or is not a directory -- exiting!" >&2
      exit 1
   fi

   if [[ -z $finality || ( $finality != jpl && $finality != docu && $finality != system ) ]]; then
      echo "You must indicate the finality ('jpl', 'docu', 'system') to subroutine clone() -- exiting" >&2
      exit 1
   fi

   if [[ $finality != system ]]; then
      if [[ -z "$which_repo" || ( $which_repo != forked && $which_repo != infra ) ]]; then
         echo "You must indicate the git repo ('forked', 'infra') to subroutine clone() -- exiting" >&2
         exit 1
      fi
   else
      # Which repo makes no sense if we are cloning for a system-wide build. Set it to "infra"
      which_repo=infra
   fi

   # cd to finality-dependent subdir

   local work_dir_fq="$toplevel_dir_fq/$finality"

   if [[ ! -d "$work_dir_fq" ]]; then
      echo "Work directory '$work_dir_fq' does not exist -- creating" >&2
      mkdir -p "$work_dir_fq" # Make with parent; no need to check whether it worked; we will try to cd to it in any case
   fi

   # >>>>>> work_dir_fq

   pushd "$work_dir_fq" >/dev/null || {
      echo "Could not cd to '$work_dir_fq' -- exiting" >&2
      exit 1
   }

   # Create a local clone of a git repository according to these settings:

   local url_and_dir
   local url
   local target_dir
   local target_dir_fq
   local modules_yesno

   url_and_dir=$(url_and_dir "$finality" "$which_repo") || {
      echo "Did not obtain valid values from url_and_dir()  -- exiting" >&2
      exit 1
   }

   url=$(echo "$url_and_dir" | cut --field=1 --delimiter='|'           ); url=$(trim "$url")
   target_dir=$(echo "$url_and_dir" | cut --field=2 --delimiter='|'    ); target_dir=$(trim "$target_dir")
   modules_yesno=$(echo "$url_and_dir" | cut --field=3 --delimiter='|' ); modules_yesno=$(trim "$modules_yesno")
   target_dir_fq="$work_dir_fq/$target_dir"

   # Break off if the target dir already exists

   if [[ -d "$target_dir_fq" ]]; then
      echo "The target directory '$target_dir_fq' already exists -- exiting" >&2
      echo "Maybe you just want to update the cloned repository?" >&2
      exit 1
   fi

   # Re-confirm with user before going on

   echo "Going to clone the remote repository at : $url" >&2
   echo "Into this directory                     : $target_dir_fq" >&2

   if ! confirm_with_user "Proceed?" ; then
      echo "Maybe later then" >&2
      exit 0
   fi

   # First clone into a temporary directory (because things may go wrong)

   local tmp_dir
   tmp_dir=$(mktemp -d -p .) || {
      echo "Could not create temporary directory in '$(pwd)' -- exiting" >&2
      exit 1
   }

   # >>>>> tmp_dir

   pushd "$tmp_dir" >/dev/null || {
      echo "Could not cd to temporary directory '$tmp_dir' -- exiting" >&2
      exit 1
   }

   # Get some clone action going!

   git clone "$url" "$target_dir" || {
      echo "Could not clone '$url' to directory '$target_dir' in '$(pwd)' -- exiting" >&2
      exit 1
   }

   if [[ $modules_yesno == modules_yes ]]; then
      cd "$target_dir" || {
         echo "Could not cd to '$target_dir' -- exiting" >&2
         exit 1
      }
      echo "Cloning submodules in '$(pwd)'" >&2
      git submodule update --init || {
         echo "Error while cloning submodules in '$(pwd)' -- exiting" >&2
         exit 1
      }
   fi

   # <<<<< tmp_dir

   popd >/dev/null || exit 1

   # We are now back in the finality-dependent directory.
   # Move the target directory to its correct location, then clean up

   mv "$tmp_dir/$target_dir" . || {
      echo "Could not move directory '$tmp_dir/$target_dir' to directory '$(pwd)' -- exiting" >&2
      exit 1
   }

   echo "Successfully cloned into directory '$target_dir_fq'" >&2

   rmdir "$tmp_dir" || {
      echo "Could not remove temporary directory '$tmp_dir' in directory '$(pwd)' -- exiting" >&2
      exit 1
   }

   # In case this is about documentation, create a symlink to the file describing builtins

   if [[ "${finality}.${which_repo}" == docu.forked ]]; then
      if [[ ! -e builtin.doc ]]; then
         ln -s "${target_dir}/man/builtin.doc" "builtin.doc"
      fi
   fi

   # <<<< work_dir_fq

   popd >/dev/null || exit 1

}

# ===========================================================================
# Copy locally
# ===========================================================================

copy() {

   local toplevel_dir_fq=${1:-}   # the directory we will be working in
   local finality=${2:-}          # "jpl" or "docu"

   if [[ -z "$toplevel_dir_fq" || ! -d "$toplevel_dir_fq" ]]; then
      echo "You must pass an existing (fully qualified) toplevel directory to subroutine copy()" >&2
      echo "but it seems '$toplevel_dir_fq' either doesn't exist or is not a directory -- exiting!" >&2
      exit 1
   fi

   if [[ -z "$finality" || ( $finality !=  jpl && $finality != docu ) ]]; then
      # There is nothing to copy at "system"
      echo "You must indicate the finality ('jpl', 'docu') to subroutine copy() -- exiting" >&2
      exit 1
   fi

   # cd to the finality-dependent subdir

   local work_dir_fq="$toplevel_dir_fq/$finality"

   # >>>>> work_dir_fq

   pushd "$work_dir_fq" >/dev/null || {
      echo "Could not cd to '$work_dir_fq' (maybe you first have to set it up using the 'clone' subcommand) -- exiting" >&2
      exit 1
   }

   # Two directories are involed: the one with the SWI-Prolog distro that can be compiled
   # and the one with modified files. We need to copy modified files from the second to the first

   local url_and_dir_infra

   url_and_dir_infra=$(url_and_dir "$finality" infra) || {
      echo "Did not obtain valid values from url_and_dir()  -- exiting" >&2
      exit 1
   }

   local url_and_dir_forked

   url_and_dir_forked=$(url_and_dir "$finality" forked) || {
      echo "Did not obtain valid values from url_and_dir()  -- exiting" >&2
      exit 1
   }

   local infra_dir
   local forked_dir

   infra_dir=$(echo "$url_and_dir_infra" | cut --field=2 --delimiter='|')   ; infra_dir=$(trim "$infra_dir")
   forked_dir=$(echo "$url_and_dir_forked" | cut --field=2 --delimiter='|') ; forked_dir=$(trim "$forked_dir")

   local exit_now=

   if [[ ! -d "$infra_dir" ]]; then
      echo "The 'infrastructure' (target) directory '$work_dir_fq/$infra_dir' does not exist -- exiting" >&2
      exit_now=1
   fi

   if [[ ! -d "$forked_dir" ]]; then
      echo "The 'forked' (source) directory '$work_dir_fq/$forked_dir' does not exist -- exiting" >&2
      exit_now=1
   fi

   if [[ -n $exit_now ]]; then
      exit 1
   fi

   # Re-confirm with user before going on

   local origin_dir="$work_dir_fq/$forked_dir"
   local target_dir="$work_dir_fq/$infra_dir"

   echo "Going to copy files from directory : '$origin_dir'" >&2
   echo "to directory                       : '$target_dir'" >&2

   declare -a from_files
   declare -a to_files

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

   # <<<< work_dir_fq

   popd >/dev/null || exit 1

}

# ===
# Directory changing code
# ===

# ---
# The function to check the presence of and pushd to a "working directory" in which we can compile
#
#
# $toplevel_dir_fq
#               |
#               +----- $finality                     $work_dir_fq (dirname is "jpl","docu","system") 
#                             |
#                             +------ $infra_dir     something like "swipl-devel_original"
# ---

dirchange_finality() {

   local toplevel_dir_fq=${1:-}
   local finality=${2:-}
   local routine_name=${3:-}

   if [[ -z "$toplevel_dir_fq" || ! -d "$toplevel_dir_fq" ]]; then
      echo "You must pass an existing (fully qualified) toplevel directory to subroutine $routine_name()" >&2
      echo "It seems directory '$toplevel_dir_fq' either doesn't exist or is not a directory -- exiting!" >&2
      exit 1
   fi

   if [[ -z "$finality" || ( $finality != jpl && $finality != docu && $finality != system ) ]]; then
      echo "You must indicate the finality ('jpl', 'docu', 'system') to subroutine $routine_name() -- exiting" >&2
      exit 1
   fi

   # cd to the finality-dependent subdir, the "work_dir_fq"
   # the work directory must exist because it contains the downloaded distribution we want to build!

   local work_dir_fq="$toplevel_dir_fq/$finality"

   pushd "$work_dir_fq" >/dev/null || {
      echo "Could not cd to '$work_dir_fq' (maybe you first have to clone the repository?) -- exiting" >&2
      exit 1
   }

   global_work_dir_fq="$work_dir_fq"  # push to global variable

}

dirchange_finality_infra() {

   local toplevel_dir_fq=${1:-}
   local finality=${2:-}
   local routine_name=${3:-}

   dirchange_finality "$toplevel_dir_fq" "$finality" "$routine_name"

   local url_and_dir_infra
   url_and_dir_infra=$(url_and_dir "$finality" infra) || {
      echo "Did not obtain valid values from url_and_dir()  -- exiting" >&2
      exit 1
   }

   local infra_dir
   infra_dir=$(echo "$url_and_dir_infra" | cut --field=2 --delimiter='|'); infra_dir=$(trim "$infra_dir")
   if [[ ! -d "$infra_dir" ]]; then
      echo "The 'infrastructure' (target) directory '$work_dir_fq/$infra_dir' does not exist -- exiting" >&2
      exit 1
   fi

   cd "$infra_dir" || {
      echo "Could not cd to '$work_dir_fq/$infra_dir' -- exiting" >&2
      exit 1
   }

   install_location=$(echo "$url_and_dir_infra" | cut --field=4 --delimiter='|')
   install_location=$(trim "$install_location")

   global_infra_dir="$infra_dir"                  # push to global variable
   global_install_location="$install_location"    # push to global variable

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
      echo "There is no VERSION file in '$work_dir_fq/$infra_dir' -- exiting" >&2
      exit 1
   fi

   global_version=$version

}

# ===========================================================================
# Checking what is checked out
# ===========================================================================

look() {

   local toplevel_dir_fq=${1:-}        # the directory we will be working in
   local finality=${2:-}               # "jpl" or "docu" or "system"

   dirchange_finality_infra "$toplevel_dir_fq" "$finality" "look"  # exits in case of problem

   local work_dir_fq="$global_work_dir_fq"  # pull from global variable
   local infra_dir="$global_infra_dir"      # pull from global variable; this is the relative repository directory

   # the above "pushd" to "work_dir_fq" and then "cd" to "infra_dir": we are in the repository directory

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
}

# ===========================================================================
# Build or rebuild an SWI-Prolog distribution that has already been
# downloaded from github.
# ===========================================================================

build() {

   local toplevel_dir_fq=${1:-}        # the directory we will be working in
   local finality=${2:-}               # "jpl" or "docu" or "system"
   local rebuild=${3:-}                # if set to "rebuild", then just "rebuild" if possible

   dirchange_finality_infra "$toplevel_dir_fq" "$finality" "build"  # exits in case of problem

   local work_dir_fq="$global_work_dir_fq"  # pull from global variable
   local infra_dir="$global_infra_dir"      # pull from global variable; this is the relative repository directory
   local install_location="$global_install_location"
   local install_dir_fq=

   # the above "pushd" to "work_dir_fq" and then "cd" to "infra_dir": we are in the repository directory

   look_for_version_file
   local version="$global_version"

   # Where to install? It depends!

   if [[ $install_location == locally ]]; then
      # relative directory based on version string
      install_dir_fq="${work_dir_fq}/swiplexe_${version}"
   else
      if [[ ! -d $install_location ]]; then
         echo "The installation location '$install_location' does not exist" >&2
         exit 1
      fi
      install_dir_fq="${install_location}/swiplexe_${version}"
   fi

   # As we are currently in the distro directory, configure & compile in here!

   local build_dir="build"

   if [[ -d $build_dir ]]; then
      if [[ $rebuild != rebuild ]]; then
         echo "Full build ordered but directory '$build_dir' already exists in '$(pwd)' -- removing it" >&2
         /bin/rm -rf "$build_dir" || {
            echo "Problems removing '$build_dir' in '$(pwd)' -- exiting" >&2
            exit 1
         }
      fi
   else
      if [[ $rebuild == rebuild ]]; then
         echo "Rebuild ordered but build directory does not exist -- performing a full build" >&2
         rebuild=xxxxxxxxxxx      # We really want to build, not rebuild
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
   local junit_jar_fq="${jar_dir_fq}/junit-4.13.jar"

   # Re-confirm with user (actually a bit late as we have already removed the build dir)

   echo "Going to build the SWI-Prolog distro in: $infra_dir"            >&2
   echo "The build directory is                 : $infra_dir/$build_dir" >&2
   echo "The installation directory is          : $install_dir_fq"       >&2
   if [[ $rebuild == rebuild ]]; then
   echo "Not performing a full build, just a rebuild" >&2
   else
   echo "Performing a full build" >&2
   fi
   if [[ -d "$jar_dir_fq" ]]; then
   echo "The following jar directory exists     : $jar_dir_fq"         >&2
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
   echo "ATTENTION! There is no jar directory   : $jar_dir_fq"         >&2
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
   look "$toplevel_dir_fq" "$finality"
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

    clone system      : To clone the original SWI Prolog repo, including submodules, in order to build for a systemwide distribution
    clone jpl forked  : To clone the modified jpl package from the personal github account, for editing
    clone jpl infra   : To clone the original SWI Prolog repo, including submodules, in order to build for jpl testing
    clone docu forked : To clone the modified SWI Prolog repo from the personal github account, for editing the documentation
    clone docu infra  : To clone the original SWI Prolog repo, including submodules, in order to build the documentation

For preparing to build

    copy (jpl,docu)           : To copy files from 
                                - the modified jpl package 
                                  to the SWI Prolog repo meant for build/testing of jpl
                                - the modified SWI Prolog for docu changes
                                  to the SWI Prolog repo meant for build/testing of documentation

For building/rebuilding

    build (system,docu,jpl)   : Configure-Build-Test-Install the distro meant for
                                - systemwide distribution if 'system' is given,
                                  (installation will have to be done manually as root);
                                - testing documentation if 'docu' is given;
                                - testing jpl if 'jpl' is given.
                                If a build already exists, it is removed.

    rebuild (system,docu,jpl) : Same as above, but without removal of existing build.

    look (system,docu,build)  : Get information about the status of the currently checked-out files.

Manual commands:

    To branch : Run something like "git checkout -b docu_202007"

TEXT

exit 1

