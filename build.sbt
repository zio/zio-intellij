import BuildHelper._

intellijPluginName in ThisBuild := "zio-intellij"
intellijBuild in ThisBuild := "192.7142.36"

lazy val root = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    name := "zio-intellij",
    version := "0.1.0",
    organization := "dev.zio.intellij",
    scalaVersion := "2.13.1",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    scalacOptions ++= ScalacOptions,
    intellijInternalPlugins := Seq("java"),
    intellijExternalPlugins += "org.intellij.scala".toPlugin,
    welcomeMessage
  )

addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("check", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
