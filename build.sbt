import sbt._

name := "scala-protobuf-java"

scalaVersion := "2.13.3"

val publishSetting = publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

val commonSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    organization := "com.github.changvvb",
    publishSetting
)

lazy val `pbconverts-macro` = project.in(file("pbconverts-macro"))
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.typelevel" %% "macro-compat" % "1.1.1",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)))
  .settings(commonSettings)
  .enablePlugins(ProtobufPlugin)

lazy val pbconverts = project.in(file("pbconverts"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test")
  .settings(commonSettings)
  .dependsOn(`pbconverts-macro`)
  .enablePlugins(ProtobufTestPlugin)

lazy val root = project.in(file(".")).withId("root")
  .aggregate(`pbconverts-macro`,pbconverts)
    .settings(commonSettings)

scalafmtOnCompile in ThisBuild := true