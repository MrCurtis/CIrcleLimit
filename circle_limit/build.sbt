enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

name := "CircleLimit root project"
scalaVersion := "2.12.8"

testFrameworks += new TestFramework("utest.runner.Framework")

resolvers ++= Seq(
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
libraryDependencies ++= Seq(
	"org.typelevel"                     %%% "spire"       % "0.14.1",
	"io.suzaku"                         %%% "diode"       % "1.1.5",
	"com.lihaoyi"                       %%% "utest"       % "0.7.1" % Test,
	"com.github.japgolly.scalajs-react" %%% "core"        % "1.4.2",
	"org.scala-js"                      %%% "scalajs-dom" % "0.9.7",
)

npmDependencies in Compile ++= Seq(
  "react" ->     "16.7.0",
  "react-dom" -> "16.7.0"
)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
