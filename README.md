# jpf-swag
Somewhat Working Assertion Generator

## System Requirements

* A recent enough version of JRE (I only tried this with Java 8)
* SBT - Simple Build Tool
* Java Pathfinder (jpf-core)
* Symbolic Pathfinder (jpf-symbc)

## Configuration and build instructions

### Let SBT be aware of JPF jars

If your jpf directory structure is as following, then you are good.

```
/
+ jpf-core/
+ jpf-symbc/
+ jpf-swag/
+ other jpf stuff
```

Otherwise, modify `base` and `baseDirectories` in definition of `unManagedJars` in `build.sbt` to point to JPF modules.

### Compile and assembly jars

Normally, we would be fine by just compiling our project with `compile` but JPF needs to be able to find Scala runtime and other possible future dependencies we might have. The current solution is to generate a fat jar that contains necessary dependencies using `assembly` command in `sbt-assembly` plugin, the SBT project is configured to install this plugin automatically. An example usage of `assembly` command is:

```
[akif@ai:~/lab/jpf/jpf-swag on master]
% sbt
[info] Loading global plugins from /Users/akif/.sbt/0.13/plugins
[info] Loading project definition from /Users/akif/lab/jpf/jpf-swag/project
[info] Set current project to jpf-swag (in build file:/Users/akif/lab/jpf/jpf-swag/)
> assembly
[info] Updating {file:/Users/akif/lab/jpf/jpf-swag/}root...
[info] Resolving jline#jline;2.12.1 ...
[info] Done updating.
[info] Compiling 1 Scala source to /Users/akif/lab/jpf/jpf-swag/target/scala-2.11/classes...
[info] Including: classloader_specific_tests.jar
[info] Including: RunTest.jar
[info] Including: scala-library-2.11.7.jar
[info] Checking every *.class/*.jar file's SHA-1.
[info] Merging files...
[warn] Merging 'META-INF/MANIFEST.MF' with strategy 'discard'
[warn] Strategy 'discard' was applied to a file
[info] SHA-1: e89bc57bab27e2f8496a4020e36ee9241bb07703
[info] Packaging /Users/akif/lab/jpf/jpf-swag/target/scala-2.11/jpf-swag.jar ...
[info] Done packaging.
[success] Total time: 16 s, completed Jan 14, 2016 12:36:46 PM
>
```

The resulting jar will be in `target/scala-2.11` directory.

### Modify native classpath on your projects.

Unfortunately JPF is quite stubborn about project structures and we need to point out our resulting jar file explicitly. One way to do it is adding (and modifying) following line to any project's `.jpf` file:

```
native_classpath=PATH_TO_JPF_SWAG/jpf-swag/target/scala-2.11/jpf-swag.jar
```

## Using jpf-swag

You need to add the listeners to your jpf configuration. You can try it out with current simple CFG listener by adding the following line to a `.jpf` file. This example listener will generate a CFG for every method whose name contains `foo`.

```
listener=edu.ucsb.cs.jpf.swag.CFGListener
```
