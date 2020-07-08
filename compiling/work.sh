#!/bin/bash

set -o nounset

toplevel_dir_fq="$HOME/_WORK_ON_PROLOG/swiplmaking2"
jplwork_dir_fq="$toplevel_dir_fq/jplwork"

local_swiplrepo_dir="swipl-devel-orig" # adapt as needed
local_jplrepo_dir="packages-jpl"       # adapt as needed

buildmark=foo

global_install_dir=

clonejpl() {

   pushd "$jplwork_dir_fq" || {
      echo "Could not cd to '$jplwork_dir_fq' -- exiting" >&2
      exit 1
   }

   if [[ -d "$local_jplrepo_dir" ]]; then
      echo "The local jpl directory '$local_jplrepo_dir' in '" $(pwd) "' already exists -- exiting" >&2
      exit 1
   fi

   local remote_fork="https://github.com/dtonhofer/packages-jpl"

   git clone "$remote_fork" "$local_jplrepo_dir" || {
      echo "Could not clone '$remote_fork' -- exiting" >&2
   }

   popd
}

cloneswipl() {

   pushd "$jplwork_dir_fq" || {
      echo "Could not cd to '$jplwork_dir_fq' -- exiting" >&2
      exit 1
   }

   if [[ -d "$local_swiplrepo_dir" ]]; then
      echo "The local SWIPL directory '$local_swiplrepo_dir' in '" $(pwd) "' already exists -- exiting" >&2
      exit 1
   fi

   local remote_repo=https://github.com/SWI-Prolog/swipl-devel

   git clone "$remote_repo" "$local_swiplrepo_dir" || {
      echo "Could not clone '$remote_repo' -- exiting" >&2
   }

   cd "$local_swiplrepo_dir" || {
      echo "Could not cd to '$local_swiplrepo_dir' in '" $(pwd) "' -- exiting" >&2
      exit 1
   }

   git submodule update --init || {
      echo "Error while updating submodules in '" $(pwd) "' -- exiting" >&2
      exit 1
   }

   popd

}

buildswipl() {

   local withtest=

   if [[ ${1:-} == "test" ]]; then
      echo "...with test" >&2
      withtest=Y
   fi

   pushd "$jplwork_dir_fq" || {
      echo "Could not cd to '$jplwork_dir_fq' -- exiting" >&2
      exit 1
   }

   # underneath the current directory, there is:

   local install_dir="$(pwd)/swipl_${buildmark}"
   local jar_dir="$(pwd)/jars"

   cd "$local_swiplrepo_dir" || {
      echo "Could not cd to '$local_swiplrepo_dir' in '" $(pwd) "' -- exiting" >&2
      exit 1
   }

   local build_dir="build_${buildmark}"

   if [[ ! -d "$build_dir" ]]; then
      echo "Directory '$build_dir' does not exist in '" $(pwd) "' -- creating" >&2
      mkdir "$build_dir"
   fi

   cd "$build_dir" || {
      echo "Could not cd to '$build_dir' in '" $(pwd) "' -- exiting" >&2
      exit 1
   }

   cmake \
      -DCMAKE_INSTALL_PREFIX="$install_dir" \
      -DHAMCREST="${jar_dir}/hamcrest-core.jar" \
      -DJUNIT_JAR="${jar_dir}/junit4.jar" \
      -DLIBEDIT_LIBRARIES=/usr/lib64/libedit.so \
      -DLIBEDIT_INCLUDE_DIR=/usr/include/editline \
      -G Ninja ..
  
   ninja       # Compile it! A lot of energy comes out of the CPU as waste heat.

   if [[ $withtest == 'Y' ]]; then
      ctest -j 4  # Run tests concurrently 4-fold. See "man ctest" or "ctest --help"
   fi

   ninja install
  
   popd

   global_install_dir="$install_dir"

}

copyjpl() {

   pushd "$jplwork_dir_fq" || {
      echo "Could not cd to '$jplwork_dir_fq' -- exiting" >&2
      exit 1
   }

   local from_dir="$local_jplrepo_dir"
   local to_dir="$local_swiplrepo_dir/packages/jpl"

   if [[ ! -d "$from_dir" ]]; then
      echo "Directory '$from_dir' does not exist in '" $(pwd) "' -- exiting" >&2
      exit 1
   fi

   if [[ ! -d "$to_dir" ]]; then
      echo "Directory '$to_dir' does not exist in '" $(pwd) "' -- exiting" >&2
      exit 1
   fi

   /bin/cp "$from_dir/jpl.pl" "$to_dir/jpl.pl"

}

# ===
# Command dispatch
# ===

cmd=${1:-}

if [[ $cmd == clonejpl ]]; then
   clonejpl
fi

if [[ $cmd == cloneswipl ]]; then
   cloneswipl
fi

if [[ $cmd == copyjpl ]]; then
   copyjpl
fi

if [[ $cmd == buildswipl ]]; then
   buildswipl withtest
   echo "Your new path should be: PATH=$global_install_dir:\$PATH"
fi

if [[ $cmd == buildswiplnotest ]]; then
   buildswipl
   echo "Your new path should be: PATH=$global_install_dir:\$PATH"
fi


