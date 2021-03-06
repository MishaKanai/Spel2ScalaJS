enablePlugins(ScalaJSPlugin)

name := "Spel2ScalaJS"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0"
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")
libraryDependencies += "io.suzaku" %%% "boopickle" % "1.3.2"
libraryDependencies += "io.circe" %%% "circe-core" % "0.13.0"
libraryDependencies += "io.circe" %%% "circe-generic" % "0.13.0"
libraryDependencies += "io.circe" %%% "circe-parser" % "0.13.0"

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

// below is for ES module output
// it produces a huge output though

// scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
// artifactPath in (Compile, fastOptJS) :=
//   (crossTarget in (Compile)).value / "spel2scalajs.mjs"

// artifactPath in (Test, fastOptJS) :=
//   (crossTarget in (Test)).value / "spel2scalajs-test.mjs"

// jsEnv := {
//   new org.scalajs.jsenv.nodejs.NodeJSEnv(
//     org.scalajs.jsenv.nodejs.NodeJSEnv
//       .Config()
//       .withArgs(List("--experimental-modules"))
//   )
// }
