#!/bin/bash

# %COPYMARK%

# SWI Prolog
# This is the directory into which self-compiled SWI Prolog has been installed.
# (The RPM package "pl" that comes with Fedora distributes files over the filetree.)

function extendPath_swipl {
   local SWIPL="/usr/local/logic/swipl"
   local BIN="${SWIPL}/bin"
   if [[ -d "${BIN}" ]]; then
      export PATH="${BIN}:${PATH}"
      export SWIPL_HOME="$SWIPL"
   fi
}

# Add the directory of "clingo" (the Potassco ASP Solver)

function extendPath_clingo {
   local CLINGO="/usr/local/logic/clingo"
   local BIN="${CLINGO}"
   if [[ -d "${BIN}" ]]; then
      export PATH="${BIN}:${PATH}"
   fi
}

# Add the directory of "Logtalk 3"

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


extendPath_swipl
# extendPath_clingo
# extendPath_logtalk3

unset extendPath_swipl
unset extendPath_logtalk3
unset extendPath_clingo


