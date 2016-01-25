lazy val commonSettings = Seq(
  organization := "edu.ucsb.cs",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "jpf-swag"
  )

unmanagedJars in Compile ++= {
    val base = baseDirectory.value / ".."
    val baseDirectories = (base / "jpf-core/build") +++ (base / "jpf-symbc/build")
    val customJars = (baseDirectories ** "*.jar")
    customJars.classpath
}

unmanagedJars in Compile += baseDirectory.value / "../jpf-symbc/lib/com.microsoft.z3.jar"

assemblyJarName in assembly := s"${name.value}.jar"

assemblyExcludedJars in assembly := { 
  val cp = (fullClasspath in assembly).value
  val excludedJars = Set(
  "jpf-classes.jar",
  "jpf-annotations.jar",
  "RunJPF.jar",
  "jpf.jar",
  "classloader_specific_tests.jar",
  "RunTest.jar"
  )
  cp filter { f =>
    excludedJars.contains(f.data.getName) ||
    f.data.getName.startsWith("jpf-")
    }
}
