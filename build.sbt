lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.1.0.6"

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
        Few important bug fixes in this release:
        <ul>
        <li>Fixed incorrect suggestions for <code>.unit</code>, other non-ZIO types (<a href="https://github.com/zio/zio-intellij/pull/79">#79</a>)</li>
        <li>Fixed incorrect <code>.when</code> simplifications (<a href="https://github.com/zio/zio-intellij/pull/82">#82</a> - thanks to @timaliberdov)</li>
        <li>Relaxed inspections of wrapping Try/Option, etc, in ZIO (<a href="https://github.com/zio/zio-intellij/pull/83">#83</a>)</li>
        <li>Made "Suggest specific ZIO type alias" to only appear on ZIO type declarations (<a href="https://github.com/zio/zio-intellij/pull/86">#86</a>)</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
