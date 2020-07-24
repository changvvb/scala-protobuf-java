import sbt._

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
    crossScalaVersions := supportedScalaVersions,
    publishSetting
)

lazy val `scala-protobuf-java-macro` = project.in(file("pbconverts-macro"))
  .settings(libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value))
  .settings(commonSettings)
  .enablePlugins(ProtobufPlugin)

lazy val `scala-protobuf-java` = project.in(file("pbconverts"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test")
  .settings(commonSettings)
  .dependsOn(`scala-protobuf-java-macro`)
  .enablePlugins(ProtobufTestPlugin)

lazy val root = project.in(file(".")).withId("root")
  .aggregate(`scala-protobuf-java`, `scala-protobuf-java-macro`)
  .settings(publishArtifact := false)

scalafmtOnCompile in ThisBuild := true
