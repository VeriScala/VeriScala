lazy val newhdl = project
  .in(file("."))
  .aggregate(lib, examples)
  .settings(common: _*)

lazy val lib = project
  .in(file("newhdl"))
  .settings(common: _*)
  .settings(name := "NewHDL",
    scalacOptions += "-language:experimental.macro",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.10.3",
      "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.2.0"))

lazy val examples = project
  .in(file("examples"))
  .dependsOn(lib)
  .settings(common: _*)
  .settings(name := "NewHDL Examples")

def common = Seq(
  organization := "com.liyaos",
  scalaVersion := "2.10.4",
  version := "0.0.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test",
  libraryDependencies += "org.scalamacros" %% "quasiquotes" % "2.0.1",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full))
