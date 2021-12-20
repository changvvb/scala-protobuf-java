import sbt._

lazy val scala213 = "2.13.6"
lazy val scala212 = "2.12.14"
lazy val scala3 = "3.1.0"

lazy val supportedScalaVersions = List(scala212, scala213, scala3)

val commonSettings = Seq(
  scalaVersion := scala3,
  scalacOptions += "-language:experimental.macros",
  organization := "com.github.changvvb",
  crossScalaVersions := supportedScalaVersions,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)


lazy val `scala-protobuf-java` = project
  .in(file("pbconverts"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test")
  .settings(libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
      case _            => Nil
    }
  })
  .settings(commonSettings)
  .enablePlugins(ProtobufTestPlugin)

lazy val root = project
  .in(file("."))
  .withId("root")
  .aggregate(`scala-protobuf-java`)
  .settings(publishArtifact := false)

ThisBuild / scalafmtOnCompile := true

ThisBuild / releasePublishArtifactsAction := releaseStepCommandAndRemaining("+publishSigned")

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

ThisBuild / credentials += Credentials(Path.userHome / ".ivy2" / ".credentials_sonatype")

Test / publishArtifact := false

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / homepage := Some(url("https://github.com/changvvb/scala-protobuf-java"))

ThisBuild / pomExtra := {
  <licenses>
        <license>
            <name>The Apache Software License, Version 2.0</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
            <distribution>repo</distribution>
        </license>
    </licenses>
      <scm>
          <connection>scm:git:git@github.com:changvvb/scala-protobuf-java.git</connection>
          <developerConnection>scm:git:git@github.com:changvvb/scala-protobuf-java.git</developerConnection>
          <url>https://github.com/changvvb/scala-protobuf-java</url>
      </scm>
      <developers>
          <developer>
              <id>changvvb</id>
              <name>Weiwei Chang</name>
              <email>changvvb@gmail.com</email>
          </developer>
      </developers>
}

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true)
ThisBuild / publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
