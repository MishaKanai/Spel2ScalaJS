enablePlugins(ScalaJSPlugin)

name := "Spel2ScalaJS"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }

artifactPath in (Compile, fastOptJS) :=
  (crossTarget in (Compile)).value / "spel2scalajs.mjs"

artifactPath in (Test, fastOptJS) :=
  (crossTarget in (Test)).value / "spel2scalajs-test.mjs"

jsEnv := {
  new org.scalajs.jsenv.nodejs.NodeJSEnv(
    org.scalajs.jsenv.nodejs.NodeJSEnv
      .Config()
      .withArgs(List("--experimental-modules"))
  )
}
