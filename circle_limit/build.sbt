import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSBundlerPlugin)

npmDependencies in Compile ++= Seq(
  "react" -> "16.7.0",
  "react-dom" -> "16.7.0"
)

name := "CircleLimit root project"

lazy val root = project.in(file(".")).
  aggregate(circleLimitJS, circleLimitJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val circleLimit = crossProject(JSPlatform, JVMPlatform).
  crossType(CrossType.Full).in(file(".")).
  settings(
    name := "circleLimit",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.9",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    resolvers ++= Seq(
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    ),
    libraryDependencies += "org.typelevel" %%% "spire" % "0.14.1",
    libraryDependencies += "io.suzaku" %%% "diode" % "1.1.5",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.1" % Test,
  ).
  jsSettings(
    libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.4.2",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  )

lazy val circleLimitJVM = circleLimit.jvm
lazy val circleLimitJS = circleLimit.js
