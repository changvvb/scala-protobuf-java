import sbt._

name := "scala-protobuf-java"

version := "0.1"

scalaVersion := "2.13.3"


lazy val `macro` = project.in(file("macro"))
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.typelevel" %% "macro-compat" % "1.1.1",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)))
  .settings(scalacOptions += "-language:experimental.macros")
  .enablePlugins(ProtobufPlugin)

lazy val core = project.in(file("core"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test")
  .settings(scalacOptions += "-language:experimental.macros")
  .dependsOn(`macro`)
  .enablePlugins(ProtobufTestPlugin)
  .enablePlugins(ProtobufPlugin)
