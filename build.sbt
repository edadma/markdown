import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.3"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.4.2"
ThisBuild / description            := "A fast, cross-platform Scala 3 CommonMark 0.31.2 Markdown parser with extensions"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

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

ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val markdown = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "markdown",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    libraryDependencies ++= Seq(
      "org.scalatest"    %%% "scalatest"      % "3.2.19" % "test",
      "com.github.scopt" %%% "scopt"          % "4.1.0",
      "com.lihaoyi"      %%% "pprint"         % "0.9.0"  % "test",
      "dev.zio"          %%% "zio-json"       % "0.7.42" % "test",
      "io.github.edadma" %%% "cross_platform" % "0.1.6",
      "io.github.edadma" %%% "logger"         % "0.0.11",
      "io.github.edadma" %%% "dllist"         % "0.0.6",
      "io.github.edadma" %%% "highlighter"    % "0.0.1" % "test",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
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
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    //  scalaJSLinkerConfig ~= { _.withModuleSplitStyle(ModuleSplitStyle.SmallestModules) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    // Library-mode: the linked .js exposes its API via @JSExportTopLevel
    // entry points (see `MarkdownJSExports.scala`), so npm consumers
    // `import { renderToHTML } from '@edadma/markdown'` rather than the
    // module being run at load time.
    scalaJSUseMainModuleInitializer        := false,
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
