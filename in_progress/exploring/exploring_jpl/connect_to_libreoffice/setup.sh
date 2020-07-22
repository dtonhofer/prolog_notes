#!/bin/bash

# Getting JPL to work and connect to LibreOffice 

# Set up variables using the script that comes with LibreOffice

source /usr/lib64/libreoffice/sdk/setsdkenv_unix.sh

# The above resets to the default JDK. Therefore:

export JAVA_HOME=/usr/local/java/jdk8_64_adopt
export PATH=$JAVA_HOME/bin:$PATH

# Avoid the mysterious error "no office executable found!"
 
# com.sun.star.comp.helper.BootstrapException: no office executable found!
#          at com.sun.star.comp.helper.Bootstrap.bootstrap(Bootstrap.java:338)
#          at com.sun.star.comp.helper.Bootstrap.bootstrap(Bootstrap.java:302)
#          at FirstUnoContact.main(FirstUnoContact.java:42)

# Code inspection shows that ridljar/com/sun/star/comp/helper/Bootstrap.java
# looks for a resource "soffice" or "soffice.exe". This means:

export CLASSPATH=/usr/lib64/libreoffice/program/:$CLASSPATH

# ---
# Java programs
# ---

# javac FirstUnoContact.java & java FirstUnoContact


