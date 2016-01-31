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
    scalaVersion := "2.11.7",
    testFrameworks += new TestFramework("utest.runner.Framework"),

    resolvers ++= Seq(
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    )
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.1",
    libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1",
    libraryDependencies += "org.spire-math" %%% "spire" % "0.11.0",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.2"
  )

lazy val circleLimitJVM = circleLimit.jvm
lazy val circleLimitJS = circleLimit.js

enablePlugins(ScalaJSPlugin)
