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
   - We are also using [Apache Maven](http://maven.apache.org/) 3.6.3 as build and execution tool.  
      - The Maven plugin which deals with unit test execution is the 
        [Maven Surefire Plugin](https://maven.apache.org/surefire/maven-surefire-plugin/)
      - I'm never really sure what Maven does and whether it supports what I want to do. We will see.
   - Maven will automatically download dependencies from the "Maven Central" repository.
      - There actually is an old jar for JPL on Maven central, dating from 2017.
      - We might look into that later.

**Stuff provided by SWI-Prolog**

If SWI Prolog has been compiled with the JPL module, there will be:

   - A JPL jar at `${SWIPL_INSTALL_DIR}/lib/swipl/lib/jpl.jar`
   - A JPL shared library at `${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/libjpl.so`
   - The library for the Prolog Virtual Machine at `${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/libswipl.so`

All of which must be made visible to the Java VM, in addition to the jar with exploited functionality.

It would be nice to have JPL as a Maven dependency, but then all of the above must be correctly packaged. 

If I find out how to do that, it might be worthwhile to follow up.

**Install Adopt OpenJDK.**

```text
$ java -version
openjdk version "14.0.1" 2020-04-14
OpenJDK Runtime Environment AdoptOpenJDK (build 14.0.1+7)
OpenJDK 64-Bit Server VM AdoptOpenJDK (build 14.0.1+7, mixed mode, sharing)
```

**Install Apache Maven**

That is very simple: Download it from [here](https://maven.apache.org/download.cgi), unpack the tarball or zip file,
then make sure the `mvn` executable (actually a shell script) is on your `PATH`, and `MAVEN_HOME` has been set to the installation directory.

**Initialize a fresh Maven project**

As described in [Maven: getting started](https://maven.apache.org/guides/getting-started/), we run the following:

```text
TOPLEVEL_DIR_NAME=callprolog
PACKAGE=name.heavycarbon.jpl.callprolog
mvn -B archetype:generate \
   -DgroupId=${PACKAGE} \
   -DartifactId=${TOPLEVEL_DIR_NAME} \
   -DarchetypeArtifactId=maven-archetype-quickstart \
   -DarchetypeVersion=1.4
```

Maven sets up a filetree that imposes a project structure. Display it with `tree`:

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
     │                       └── App.java <--- Placeholder code; contains a main that prints 'Hello World'; will not be used for now
     └── test
         └── java <------------- Java sources for test code go here
             └── name
                 └── heavycarbon
                     └── jpl
                         └── callprolog
                             └── AppTest.java <--- Placeholder code; we will write tests here
```

Files `App.java` and `AppTest.java` are replaced as required. 

**Modify the POM**

We will now modify the [Project Object Model (POM)](https://maven.apache.org/guides/introduction/introduction-to-the-pom.html) file `pom.xml`.

   - Compiler source and target should be at version "14", not "1.7" as set by default
   - "Maven Surefire" is the plugin which is supposed to run the tests. According to
     [Maven Surefire Plugin - Using JUnit 5 Platform](https://maven.apache.org/surefire/maven-surefire-plugin/examples/junit-platform.html)
     we have to add dependencies for:
      - `org.junit.jupiter/junit-jupiter-engine` 5.7.0 (scoped to "test") ([Maven repo entry](https://mvnrepository.com/artifact/org.junit.jupiter/junit-jupiter-engine))
      - `org.junit.vintage/junit-vintage-engine` 5.7.0 (scoped to "test") ([Maven repo entry](https://mvnrepository.com/artifact/org.junit.vintage/junit-vintage-engine))
   - Additionally, we need dependencies for:
      - `org.slf4j/slf4j-api` 2.0.0-alpha1 ([Maven repo entry](https://mvnrepository.com/artifact/org.slf4j/slf4j-api)), the logging shim.      
      - `org.hamcrest/hamcrest-all` 1.3 ([Maven repo entry](https://mvnrepository.com/artifact/org.hamcrest/hamcrest-all)), to write down matching expression for tests
      - `ch.qos.logback/logback-classic` 1.3.0-alpha5 ([Maven repo entry](https://mvnrepository.com/artifact/ch.qos.logback/logback-classic)), the logging framework implementation  
   
Based on the above, Maven will download all the necessary jars and transitively and jars those jars depend on.

Additionally, we want to run JUnit 5 tests from the console (not only through Maven). For this we need the "junit-platform-console-standalone" jar. It will not be used by Maven but will be found in the Maven cache, from where we can use it directly. 

Therefore, a dependency for `org.junit.platform/junit-platform-console-standalone` 1.7.0 ([Maven repo entry](https://mvnrepository.com/artifact/org.junit.platform/junit-platform-console-standalone)) is added

For compiling, we need a dependency on `jpl.jar`. The one in Maven Central may be good, or not. We will use the one in our compiled SWI Prolog distrobution. The way to do this correctly is to set up a local Maven Repository from where the jar can be downloaded. To be done! For now, it suffices to add this

Define a variable in block `properties`:

```
<swi-prolog.install.dir>/usr/local/logic/swiplexe_8.3.7</swi-prolog.install.dir>
```

Then add this dependency:

```
<dependency>
    <groupId>org.jpl</groupId>
    <artifactId>JPL</artifactId>
    <version>1.0</version>
    <scope>system</scope>
    <systemPath>${swi-prolog.install.dir}/lib/swipl/lib/jpl.jar</systemPath>
</dependency>
```
   
- The default dependencies generated by Maven initialization look a bit outdated. To be reviewed later.

**Correct the Surefire Plugin entry in the POM**

The Surefire Plugin can be found [Maven repo entry](https://mvnrepository.com/artifact/org.apache.maven.plugins/maven-surefire-plugin).

   - [Intro](https://maven.apache.org/surefire/maven-surefire-plugin/index.html)
   - [Usage](https://maven.apache.org/surefire/maven-surefire-plugin/usage.html)

The Surefire Plugin entry is outdated:

```
<plugin>
   <artifactId>maven-surefire-plugin</artifactId>
   <version>2.22.1</version>
</plugin>
```

Replace it with

```
<plugin>
   <groupId>org.apache.maven.plugins</groupId>
   <artifactId>maven-surefire-plugin</artifactId>
   <version>3.0.0-M5</version>
   <!-- this is needed for running it -->
   <configuration>
      <additionalClasspathElements>
         <!-- <additionalClasspathElement>${swi-prolog.install.dir}/lib/swipl/lib/jpl.jar</additionalClasspathElement> -->
      </additionalClasspathElements>
   </configuration>
</plugin>
```

This might do...

**Write a simple test**

```
package name.heavycarbon.jpl.callprolog;

import static org.junit.Assert.*;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jpl7.JPL;

public class ConnectTest {

    @Test
    public void gettingSettingDefaultInitArgs() {
       Logger logger = LoggerFactory.getLogger("gettingSettingDefaultInitArgs");
       // String[] initArgs = JPL.getActualInitArgs(); // returns null if not yet initialized
       String[] initArgs = JPL.getDefaultInitArgs(); // returns null if not yet initialized
       for (String str : initArgs) {
          logger.info("Found {}",str);
       }
    }

}
```

**Run tests with Maven**

```
$ mvn test
```

This will fail because the shared object library cannot be found:

```
java.lang.UnsatisfiedLinkError: no jpl in java.library.path: [/usr/java/packages/lib, /usr/lib64, /lib64, /lib, /usr/lib]
```

I'm not sure how to solve this.

But let's run it with the JUnit Standalone Runner instead.

**Run tests with JUnit standalone runner**

Create directory "jars" directly under `callprolog` to collect symlinks to jars we need, which we pick out of the Maven cache in `$HOME/.m2`

```
SWIPL_INSTALL_DIR=/usr/local/logic/swiplexe_8.3.7
ln -s "$SWIPL_INSTALL_DIR/lib/swipl/lib/jpl.jar"                                                          jpl.jar 
ln -s "$HOME/.m2/repository/ch/qos/logback/logback-classic/1.3.0-alpha5/logback-classic-1.3.0-alpha5.jar" logback-classic.jar
ln -s "$HOME/.m2/repository/ch/qos/logback/logback-core/1.3.0-alpha5/logback-core-1.3.0-alpha5.jar"       logback-core.jar
ln -s "$HOME/.m2/repository/org/slf4j/slf4j-api/2.0.0-alpha1/slf4j-api-2.0.0-alpha1.jar"                  slf4j-api.jar
ln -s "$HOME/.m2/repository/org/junit/platform/junit-platform-console-standalone/1.7.0/junit-platform-console-standalone-1.7.0.jar" junitsl.jar
```

Set the `CLASSPATH`. Note the entry `$HERE/target/test-classes` which is where test class compilates are found.

```
HERE=$(pwd)
export CLASSPATH=\
$HERE/target/test-classes:\
$HERE/jars/jpl.jar:\
$HERE/jars/junitsl.jar:\
$HERE/jars/logback-classic.jar:\
$HERE/jars/logback-core.jar:\
$HERE/jars/slf4j-api.jar:\
$CLASSPATH
```

The try to run the test with the standalone runner:

```
java -Djava.library.path=${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/ \
     -jar jars/junitsl.jar \
     --classpath $CLASSPATH \
     --scan-classpath
```

This works but is awkward. Everything should run in Maven directly.

```
$ java -Djava.library.path=${SWIPL_INSTALL_DIR}/lib/swipl/lib/x86_64-linux/      -jar jars/junitsl.jar      --classpath $CLASSPATH      --scan-classpath
23:48:49.751 [main] INFO gettingSettingDefaultInitArgs - Found swipl
23:48:49.754 [main] INFO gettingSettingDefaultInitArgs - Found -g
23:48:49.755 [main] INFO gettingSettingDefaultInitArgs - Found true
23:48:49.755 [main] INFO gettingSettingDefaultInitArgs - Found --no-signals

Thanks for using JUnit! Support its development at https://junit.org/sponsoring

╷
├─ JUnit Jupiter ✔
└─ JUnit Vintage ✔
   └─ ConnectTest ✔
      └─ gettingSettingDefaultInitArgs ✔

Test run finished after 150 ms
[         3 containers found      ]
[         0 containers skipped    ]
[         3 containers started    ]
[         0 containers aborted    ]
[         3 containers successful ]
[         0 containers failed     ]
[         1 tests found           ]
[         0 tests skipped         ]
[         1 tests started         ]
[         0 tests aborted         ]
[         1 tests successful      ]
[         0 tests failed          ]
```
