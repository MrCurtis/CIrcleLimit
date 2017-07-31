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
    ),
    libraryDependencies += "org.spire-math" %%% "spire" % "0.11.0",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.5" % Test,
    libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "3.4.0" % Test
  ).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.3",
  )

lazy val circleLimitJVM = circleLimit.jvm
lazy val circleLimitJS = circleLimit.js

enablePlugins(ScalaJSPlugin)
