name := "markdown"

version := "0.3.2"

scalaVersion := "2.12.6"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "http://hyperreal.ca/maven2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
)

libraryDependencies ++= Seq(
  "xyz.hyperreal" %% "highlighter" % "0.1.1"
//  "xyz.hyperreal" %% "yaml" % "0.1.4"
)

mainClass in (Compile, run) := Some( "xyz.hyperreal.markdown.Main" )

mainClass in assembly := Some( "xyz.hyperreal.markdown.Main" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"


publishMavenStyle := true

//publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

//{
//  val nexus = "https://oss.sonatype.org/"
//  if (isSnapshot.value)
//    Some("snapshots" at nexus + "content/repositories/snapshots")
//  else
//    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
//}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/markdown"))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/markdown.git</url>
    <connection>scm:git:git@github.com:edadma/markdown.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://hyperreal.ca</url>
    </developer>
  </developers>
