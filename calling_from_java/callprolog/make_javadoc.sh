#!/bin/bash

here=$(dirname "$0")

javadocs="$here/javadocs"

cd "$javadocs" || {
   echo "Could not cd to directory 'javadocs' -- exiting" >&2
   exit 1
}

javadoc -sourcepath ../javasrc/ org.jpl7


