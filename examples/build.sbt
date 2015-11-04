lazy val lib = ProjectRef(file("../newhdl"), "lib")

lazy val examples = project
  .in(file("."))
  .dependsOn(lib)
  .settings(name := "NewHDL Examples",
    organization := "com.liyaos",
    scalaVersion := "2.11.7",
    version := "0.0.1",
    parallelExecution in Test := false,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "com.typesafe" % "config" % "1.3.0"))
