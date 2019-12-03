import BuildHelper._

intellijPluginName in ThisBuild := "zio-intellij"

// Repository: https://www.jetbrains.com/intellij-repository/releases
intellijBuild in ThisBuild := "193.5233.102" // 2019.3

val ScalaVersion = "2.12.10"

lazy val root = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    name := "zio-intellij",
    version := "0.1.0",
    organization := "dev.zio.intellij",
    scalaVersion := ScalaVersion,
    javacOptions in Global ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked"),
    scalacOptions in Global ++= Seq("-target:jvm-1.8", "-deprecation"),
    scalacOptions ++= ScalacOptions,
    intellijInternalPlugins := Seq("java"),
    intellijExternalPlugins += "org.intellij.scala:2019.3.17".toPlugin,
//    packageLibraryMappings := Seq.empty,
    packageLibraryMappings += "org.scala-lang" % "scala-library" % ScalaVersion -> Some("lib/scala-library.jar"),
    welcomeMessage
  )

lazy val runner = createRunnerProject(root, "plugin-runner")

addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("check", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
