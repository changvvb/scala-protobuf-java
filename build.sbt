import sbt._

name := "scala-protobuf-java"


lazy val scala213 = "2.13.3"
lazy val scala212 = "2.12.10"

lazy val supportedScalaVersions = List(scala212, scala213)


val publishSetting = publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

val commonSettings = Seq(
    scalaVersion := scala213,
    scalacOptions += "-language:experimental.macros",
    organization := "com.github.changvvb",
    publishSetting
)

lazy val `pbconverts-macro` = project.in(file("pbconverts-macro"))
  .settings(libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value))
  .settings(commonSettings)
  .settings(crossScalaVersions := supportedScalaVersions)
  .enablePlugins(ProtobufPlugin)

lazy val pbconverts = project.in(file("pbconverts"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test")
  .settings(commonSettings)
  .dependsOn(`pbconverts-macro`)
  .settings(crossScalaVersions := supportedScalaVersions)
  .enablePlugins(ProtobufTestPlugin)

lazy val root = project.in(file(".")).withId("root")
  .aggregate(`pbconverts-macro`,pbconverts)
    .settings(commonSettings)

scalafmtOnCompile in ThisBuild := true