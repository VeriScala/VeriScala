lazy val newhdl = project
  .in(file("."))
  .aggregate(lib, examples)
  .settings(common: _*)

lazy val lib = ProjectRef(file("newhdl"), "lib")

lazy val examples = ProjectRef(file("examples"), "examples")

def common = Seq(
  organization := "com.liyaos",
  scalaVersion := "2.11.7",
  version := "0.0.1",
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.1" % "test"))
