lazy val root = (project in file(".")).
  settings(
    name := "circle_limit",
    version := "0.0",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.1",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    libraryDependencies  ++= Seq(
      // other dependencies here
      "org.scalanlp" %% "breeze" % "0.10",
      // native libraries are not included by default. add this if you want them (as of 0.7)
      // native libraries greatly improve performance, but increase jar sizes.
      "org.scalanlp" %% "breeze-natives" % "0.10"
    ),

    resolvers ++= Seq(
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    ),
    scalaVersion := "2.11.1" // or 2.10.3 or later
  )

