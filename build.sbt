lazy val newhdl = project
  .in(file("."))
  .aggregate(lib, examples)
  .settings(common: _*)

lazy val lib = project
  .in(file("newhdl"))
  .settings(common: _*)
  .settings(name := "NewHDL",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value))

lazy val examples = project
  .in(file("examples"))
  .dependsOn(lib)
  .settings(common: _*)
  .settings(name := "NewHDL Examples")

def common = Seq(
  organization := "com.liyaos",
  scalaVersion := "2.11.0",
  version := "0.0.1",
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.1" % "test"))
