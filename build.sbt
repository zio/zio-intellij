import BuildHelper._

intellijPluginName in ThisBuild := "zio-intellij"
intellijBuild in ThisBuild := "192.7142.36" // 2019.2
//intellijBuild in ThisBuild := "193.4778.7" // 2019.3

val ScalaVersion = "2.12.10"

lazy val root = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    name := "zio-intellij",
    version := "0.1.0",
    organization := "dev.zio.intellij",
    scalaVersion := ScalaVersion,
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    scalacOptions ++= ScalacOptions,
    intellijInternalPlugins := Seq("java"),
    intellijExternalPlugins += "org.intellij.scala".toPlugin,
//    packageLibraryMappings := Seq.empty,
    packageLibraryMappings += "org.scala-lang" % "scala-library" % ScalaVersion -> Some("lib/scala-library.jar"),
    welcomeMessage
  )

lazy val runner = createRunnerProject(root, "plugin-runner")

addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("check", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
