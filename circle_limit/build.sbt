lazy val root = (project in file(".")).
  settings(
    name := "circle_limit",
    version := "0.0",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.1",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    libraryDependencies += "org.spire-math" %% "spire" % "0.10.1"
  )

