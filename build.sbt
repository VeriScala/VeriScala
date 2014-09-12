lazy val newhdl = project
  .in(file("."))
  .aggregate(lib, examples)
  .settings(common: _*)

lazy val lib = project
  .in(file("newhdl"))
  .settings(common: _*)
  .settings(name := "NewHDL",
    scalacOptions += "-language:experimental.macro",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.3")

lazy val examples = project
  .in(file("examples"))
  .dependsOn(lib)
  .settings(common: _*)
  .settings(name := "NewHDL Examples")

def common = Seq(
  organization := "com.liyaos",
  scalaVersion := "2.10.3",
  version := "0.0.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test")
