publishMavenStyle := true

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials_sonatype")

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

homepage := Some(url("https://github.com/changvvb/scala-protobuf-java"))

pomExtra := {
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

publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)