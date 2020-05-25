lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.1.0.7"

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "2020.1"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

lazy val `zio-intellij` = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    scalaVersion := scala212,
    version := pluginVersion,
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala:2020.1.29".toPlugin
    ),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = s"""<![CDATA[
        A tiny bugfix release:
        <ul>
        <li>Fixed incorrect suggestions for <code>ZIO.service</code> (<a href="https://github.com/zio/zio-intellij/pull/97">#97</a> - big thanks to @timaliberdov)</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
