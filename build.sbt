import BuildHelper._

val ScalaVersion  = "2.12.10"
val ZioVersion    = "1.0.0-RC17"
val PluginVersion = "2019.3.0.1"

lazy val root = project
  .in(file("."))
  .aggregate(plugin, testrunner)
  .enablePlugins(SbtIdeaPlugin)
  .settings(org.jetbrains.sbtidea.Keys.buildSettings)
  .settings(
    name := "zio-intellij",
    version := PluginVersion,
    organization := "dev.zio.intellij",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    scalaVersion := ScalaVersion,
    javacOptions in Global ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked"),
    scalacOptions in Global ++= Seq("-target:jvm-1.8", "-deprecation"),
    scalacOptions ++= ScalacOptions,
    crossScalaVersions := Nil,
    autoScalaLibrary := false,
    intellijPluginName := "zio-intellij",
    intellijPlatform := IntelliJPlatform.IdeaCommunity,
    intellijBuild := "193.5233.102", // 2019.3, see https://www.jetbrains.com/intellij-repository/releases
    intellijMainJars ++= maybeToolsJar,
    welcomeMessage
  )

lazy val plugin = project
  .in(file("plugin"))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
      intellijInternalPlugins += "java",
      intellijExternalPlugins += "org.intellij.scala:2019.3.23".toPlugin,
  )

lazy val testrunner = project
  .in(file("testrunner"))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    name := "testrunner",
    version := PluginVersion,
    scalaVersion := ScalaVersion,
    crossScalaVersions := List("2.12.10", "2.13.1"),
    intellijExternalPlugins += "org.intellij.scala:2019.3.23".toPlugin,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"      % ZioVersion,
      "dev.zio" %% "zio-test" % ZioVersion
    )
  )

lazy val runner = createRunnerProject(root, "plugin-runner")

addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("check", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
