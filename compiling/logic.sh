#!/bin/bash

# %COPYMARK%

# ---
# This script is copied into /etc/profile.d so that it is run whenever bash
# is started. It extends the PATH with the bin directory of the (current)
# SWI-Prolog distribution (found in /usr/local/logic/swipl in this case)
# and also sets the SWIPL_HOME environment variable.
#
# Additionally, it sets the path to "clingo" (the Potsdam ASP Solver) if
# it exists, and the path to Logtalk (which is Prolog++: https://logtalk.org/)
# if it exists
# ---

# ---
# /usr/local/logic/swipl is the directory into which my self-compiled
# SWI-Prolog has been installed.
#
# The corresponding RPM package "pl" that comes with Fedora distributes
# files over the filetree. On 2021-04-21 the package "pl" is at 8.2.4
# whereas devel version of SWI-Prolog is at 8.3.22
#
# $ dnf info pl
# pl.x86_64 : SWI-Prolog - Edinburgh compatible Prolog compiler
# ---

function extendPath_swipl {
   local SWIPL="/usr/local/logic/swipl"
   local BIN="${SWIPL}/bin"
   if [[ -d "${BIN}" ]]; then
      export PATH="${BIN}:${PATH}"
      export SWIPL_HOME="$SWIPL"
   fi
}

function extendPath_clingo {
   local CLINGO="/usr/local/logic/clingo"
   local BIN="${CLINGO}"
   if [[ -d "${BIN}" ]]; then
      export PATH="${BIN}:${PATH}"
   fi
}

function extendPath_logtalk3 {
   local WHERE=/usr/local/logic
   export LOGTALKHOME="$WHERE/lgt3git"
   export LOGTALKUSER="$WHERE/lgt3git"
   export PATH="$PATH:$LOGTALKHOME/tools/diagrams"
          PATH="$PATH:$LOGTALKHOME/tools/lgtdoc/xml"
          PATH="$PATH:$LOGTALKHOME/scripts"
          PATH="$PATH:$LOGTALKHOME/integration"
   # MANPATH is generally no longer used
   export MANPATH="$MANPATH:$LOGTALKHOME/man"
}

# Call some of the above functions, then drop the functions again

extendPath_swipl
# extendPath_clingo
# extendPath_logtalk3

# Drop the functions again

unset extendPath_swipl
unset extendPath_clingo
unset extendPath_logtalk3
