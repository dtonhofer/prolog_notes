#!/bin/bash

# ===
# Run Prolog "plunit" test blocks found in files listed in this very script.
# This script passes https://www.shellcheck.net/
# ronerycoder@gluino.name
# ===

set -o nounset

# ---
# Get the directory of this script
# ---

mydir=$(dirname "$0")

# ---
# Command line processing
# Accepts "--quiet" to squelch plunit ouput
# ---

quietflag=${1:-}  # first flag on command line, if it exists
quietgoal=        # Prolog goal that sets the "silent plunit test" flag

if [[ $quietflag == '--quiet' ]]; then
   quietgoal="set_test_options([silent(true)])"
else
   quietgoal=true
fi

# ---
# Is "swipl" around?
# ---

swipl=$(which swipl 2>/dev/null) || {
   echo "Didn't find the 'swipl' command -- exiting" >&2
   exit
}

version=$("$swipl" --version) || {
   echo "Didn't obtain version from '$swipl' -- exiting" >&2
   exit 1
}

echo "We are using this: '$version'" >&2

# ---
# Listing files to execute
# ---

# Explicit list. Later we can make this list dynamic by using:
# find . -maxdepth 1 -iname "*.pl" -exec basename '{}' ';'

test_files=(
   simplest/test_succeed.pl
   #simplest/test_fail.pl
   #simplest/test_throw.pl
   #simplest/test_partially_succeed.pl
   simplest/tests_demonstrating_units_tests.pl
   builtin_demo/test_atom_string.pl
   builtin_demo/test_between.pl
   builtin_demo/test_compound_name_arguments.pl
   builtin_demo/test_compound_name_arity.pl
   builtin_demo/test_length.pl   
   builtin_demo/test_is_list.pl
   builtin_demo/test_atom_codes.pl
   builtin_demo/test_dicts.pl
   )

# TODO: Output files should be put into a temporary directory

for test_file in "${test_files[@]}"; do
   test_file_q="$mydir/$test_file"
   if [[ ! -f "$test_file_q" ]]; then
      echo "Skipping '$test_file' because there is no such file" >&2
      continue
   fi
   # test_file_dir=$(dirname "$test_file_q")
   test_file_base=$(basename "$test_file_q")
   tmp_file="output_${test_file_base}_$(date +%Y-%m-%dT%H:%M:%S).txt"
   touch "$tmp_file"  || {
      echo "Couldn't create a temporary file to take up output -- exiting" >&2
      exit 1
   }
   echo -n "Running '$test_file'" >&2
   # Call halt with exit value 0 on success, exit value 1 on failure
   "$swipl" -g "$quietgoal , (run_tests -> halt(0) ; halt(1))" "$test_file_q" 1>"$tmp_file" 2>&1
   res=$?
   echo -n "...return value is $res." >&2
   if [[ $res -eq 0 ]]; then
      /bin/rm "$tmp_file"
      echo >&2
   else
      echo " Execution failed. Output is in file '$tmp_file'" >&2
   fi
done

