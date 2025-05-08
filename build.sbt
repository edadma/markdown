ThisBuild / licenses += "ISC"      -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.6.4"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.19"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
)
ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots") ++ Resolver.sonatypeOssRepos("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/markdown"),
    "scm:git@github.com:edadma/markdown.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage := Some(url("https://github.com/edadma/markdown"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

lazy val markdown = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "markdown",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xfatal-warnings",
      ),
    organization := "io.github.edadma",
    libraryDependencies ++= Seq(
      "org.scalatest"    %%% "scalatest"      % "3.2.19" % "test",
      "com.github.scopt" %%% "scopt"          % "4.1.0",
      "com.lihaoyi"      %%% "pprint"         % "0.9.0"  % "test",
      "dev.zio"          %%% "zio-json"       % "0.7.42" % "test",
      "io.github.edadma" %%% "cross-platform" % "0.0.3"  % "test",
      "io.github.edadma" %%% "logger"         % "0.0.11",
      "io.github.edadma" %%% "dllist"         % "0.0.6",
      "io.github.edadma" %%% "cross-platform" % "0.0.3",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    licenses += "ISC"      -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    libraryDependencies += "org.scala-js"       %% "scalajs-stubs"   % "1.1.0" % "provided",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    Test / scalaJSUseMainModuleInitializer := true,
    Test / scalaJSUseTestModuleInitializer := false,
//    Test / scalaJSUseMainModuleInitializer      := false,
//    Test / scalaJSUseTestModuleInitializer      := true,
    scalaJSUseMainModuleInitializer             := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )

lazy val root = project
  .in(file("."))
  .aggregate(markdown.js, markdown.jvm, markdown.native)
  .settings(
    name                := "markdown",
    publish / skip      := true,
    publishLocal / skip := true,
  )
