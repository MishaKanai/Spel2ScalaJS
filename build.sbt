enablePlugins(ScalaJSPlugin)

name := "Spel2ScalaJS"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }

jsEnv := {
  new org.scalajs.jsenv.nodejs.NodeJSEnv(
    org.scalajs.jsenv.nodejs.NodeJSEnv
      .Config()
      .withArgs(List("--experimental-modules"))
  )
}
