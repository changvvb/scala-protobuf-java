import sbt._

name := "scala-protobuf-java"

version := "0.1"

scalaVersion := "2.13.3"

val commonSettings = Seq(
    scalacOptions += "-language:experimental.macros"
)

lazy val `macro` = project.in(file("pbconverts-macro"))
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.typelevel" %% "macro-compat" % "1.1.1",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)))
  .settings(commonSettings)
  .enablePlugins(ProtobufPlugin)

lazy val core = project.in(file("pbconverts"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test")
  .settings(commonSettings)
  .dependsOn(`macro`)
  .enablePlugins(ProtobufTestPlugin)

lazy val root = project.in(file(".")).withId("root")
  .aggregate(`macro`,core)

scalafmtOnCompile in ThisBuild := true