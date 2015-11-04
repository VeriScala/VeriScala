lazy val lib = project
  .in(file("."))
  .settings(name := "NewHDL",
    organization := "com.liyaos",
    scalaVersion := "2.11.7",
    version := "0.0.1",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "com.typesafe" % "config" % "1.3.0"))
