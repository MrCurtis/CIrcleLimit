name := "CircleLimit root project"

lazy val root = project.in(file(".")).
  aggregate(circleLimitJS, circleLimitJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val circleLimit = crossProject.in(file(".")).
  settings(
    name := "circleLimit",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.5",
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
    )
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.1"
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1"
  )

lazy val circleLimitJVM = circleLimit.jvm
lazy val circleLimitJS = circleLimit.js

enablePlugins(ScalaJSPlugin)
