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
    scalaVersion := "2.12.4",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    resolvers ++= Seq(
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    ),
    libraryDependencies += "org.typelevel" %%% "spire" % "0.14.1",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.0" % Test,
    libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "3.7.1" % Test
  ).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
  )

lazy val circleLimitJVM = circleLimit.jvm
lazy val circleLimitJS = circleLimit.js

enablePlugins(ScalaJSPlugin)
