import sbt._

lazy val scala213 = "2.13.3"
lazy val scala212 = "2.12.10"

lazy val supportedScalaVersions = List(scala212, scala213)

val commonSettings = Seq(
    scalaVersion := scala213,
    scalacOptions += "-language:experimental.macros",
    organization := "com.github.changvvb",
    crossScalaVersions := supportedScalaVersions,
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

// publish

releasePublishArtifactsAction in ThisBuild := releaseStepCommandAndRemaining("+publishSigned")

publishTo in ThisBuild := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle in ThisBuild := true

credentials in ThisBuild  += Credentials(Path.userHome / ".ivy2" / ".credentials_sonatype")

publishArtifact in Test := false

pomIncludeRepository in ThisBuild  := { _ => false }

homepage in ThisBuild  := Some(url("https://github.com/changvvb/scala-protobuf-java"))

pomExtra in ThisBuild  := {
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

publishConfiguration in ThisBuild  := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration in ThisBuild  := publishLocalConfiguration.value.withOverwrite(true)