# Calling SWI-Prolog from Java (via JPL)

**2020-09-25: Work in progress, extremely incomplete**
   
   - JPL documentation can be found at:
      - The [JPL project website](http://jpl7.org/)
         - The [JPL API overview](https://jpl7.org/JavaApiOverview)
         - The [The JPL javadoc](https://jpl7.org/javadoc/index.html)      
      - The [The wiki page of the JPL Github Repository](https://github.com/ssardina-research/packages-jpl/wiki)
      - The [JPL page of the SWI-Prolog reference manual](https://eu.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/jpl.html%27%29)
   - We are using SWI Prolog 8.3.7
   - We are using Java 14, in this case [Adopt OpenJDK](https://adoptopenjdk.net/) on Linux x64.
      - The latest version is actually Java 15 already.
   - For logging, we will use [SLF4J](http://www.slf4j.org/), the "Simple Logging Façade for Java". This is a shim
     which allows one to write to a single API and "plug in" one or the other of logging implementations at 
     configuration time, just by dropping the correct jar onto the CLASSPATH.
      - The logging implementation we use will be [logback](http://logback.qos.ch/).
   - To write and run test, we are using [JUnit 5](https://junit.org/junit5/). This gives us an excellent
     framework to develop smallish code pieces to exercise features. The part of Junit5 which provides
     the Junit5-specific features is called "Junit Jupiter".
   - We are also using [Apache Maven](http://maven.apache.org/) as build and execution tool.  
      - The Maven plugin which deals with unit test execution is the 
        [Maven Surefire Plugin](https://maven.apache.org/surefire/maven-surefire-plugin/)
      - I'm never really sure what Maven does and whether it supports what I want to do. We will see.
   - Maven will automatically download dependencies from the "Maven Central" repository.
      - There actually is an old jar for JPL on Maven central, dating from 2017.
      - We might look into that later.
So

```text
$ java -version
openjdk version "14.0.1" 2020-04-14
OpenJDK Runtime Environment AdoptOpenJDK (build 14.0.1+7)
OpenJDK 64-Bit Server VM AdoptOpenJDK (build 14.0.1+7, mixed mode, sharing)
```

First, we need to initialize a fresh Maven project.

According to [Maven: getting started](https://maven.apache.org/guides/getting-started/), we run the following:

```text
TOPLEVEL_DIR_NAME=callprolog
PACKAGE=name.heavycarbon.jpl.callprolog
mvn -B archetype:generate \
   -DgroupId=${PACKAGE} \
   -DartifactId=${TOPLEVEL_DIR_NAME} \
   -DarchetypeArtifactId=maven-archetype-quickstart \
   -DarchetypeVersion=1.4
```

A filetree is set up. Display it with `tree`:

```
    callprolog
    ├── pom.xml <----------------- Maven project object model (config) file 
    └── src
        ├── main
        │   └── java <------------ Java sources for production code go here
        │       └── name
        │           └── heavycarbon
        │               └── jpl
        │                   └── callprolog
        │                       └── App.java <--- Placeholder code; will not be used for now
        └── test
            └── java <------------- Java sources for test code go here
                └── name
                    └── heavycarbon
                        └── jpl
                            └── callprolog
                                └── AppTest.java <--- Placeholder code; we will write tests here
```

We will now modify the [Project Object Model (POM)](https://maven.apache.org/guides/introduction/introduction-to-the-pom.html) file.

   - Compiler source and target should be at version "14", not "1.7" as set by default
   - Add dependencies on
        `org.junit.jupiter/junit-jupiter-engine` 5.7.0 (scoped to "test")
        `org.junit.vintage/junit-vintage-engine` 5.7.0 (scoped to "test")
   - The default dependencies look a bit outdated. To be reviewed later.


If SWI Prolog has been compiled with the JPL module, there will be:

   - A JPL jar at `${SWIPL_INSTALL_DIR}/lib/swipl/lib/jpl.jar`
   - A JPL shared library at `${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/libjpl.so`
   - The library for the Prolog Virtual Machine at `${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/libswipl.so`

All of which must be made visible to the Java VM, in addition to the jar with exploited functionality.

It would be nice to have JPL as a Maven dependency, but then all of the above must be correctly packaged. 
If I find out how to do that, it might be worthwhile to follow up.

...After lengthy trials to be explained ....

With

```
cl.jar -> $HOME/.m2/repository/org/junit/platform/junit-platform-console-standalone/1.7.0/junit-platform-console-standalone-1.7.0.jar
```

and

```
CLASSPATH=target/test-classes/:slf4j-api-2.0.0-alpha1.jar:logback-core-1.3.0-alpha5.jar:logback-classic-1.3.0-alpha5.jar:jpl.jar
```

and

```
SWIPL_INSTALL_DIR=/usr/local/logic/swiplexe_8.3.7
```

tests can be run from the commandline uing the "JUnit Console Standalone Runner":

```
java -Djava.library.path=${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/ \
     -jar cl.jar \
     --classpath $CLASSPATH \
     --scan-classpath
```


