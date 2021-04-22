#!/bin/bash

# Additional options can be passed to the compiler by adding a 
# line like this (sic):
#
# -cc-options,-std=gnu2x \

object="sayhellolib.so"
source="sayhellolib.c"

object_dir="lib"
source_dir="src"

from_source="$source_dir/$source"
to_object="$object_dir/$object"

if [[ ! -f "$from_source" ]]; then
   echo "Source '$from_source' does not exist -- exiting" >&2
   exit 1
fi

if [[ ! -d "$object_dir" ]]; then
   echo "Target directory '$object_dir' does not exist -- exiting" >&2
   exit 1
fi

swipl-ld \
  -v \
  -shared \
  -o "$to_object" \
  "$from_source"

# Print symbols for fun

nm "$to_object" 
