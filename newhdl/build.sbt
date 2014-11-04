lazy val lib = project
  .in(file("."))
  .settings(name := "NewHDL",
    organization := "com.liyaos",
    scalaVersion := "2.11.0",
    version := "0.0.1",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalatest" %% "scalatest" % "2.2.1" % "test"))
