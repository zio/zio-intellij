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
        Few important bug fixes and new features in this release:
        <ul>
        <li>Added a new simplification for <code>ZIO.access(_.get) -> ZIO.service</code> (<a href="https://github.com/zio/zio-intellij/pull/92">#92</a> - big thanks to @timaliberdov)</li>
        <li>The test runner now supports running entire <code>suite</code>s (<a href="https://github.com/zio/zio-intellij/pull/94">#94</a>)</li>
        <li>Fixed incorrect suggestions for <code>.unit</code>, other non-ZIO types (<a href="https://github.com/zio/zio-intellij/pull/79">#79</a>)</li>
        <li>Fixed incorrect <code>.when</code> simplifications (<a href="https://github.com/zio/zio-intellij/pull/82">#82</a> - thanks to @timaliberdov)</li>
        <li>Relaxed inspections of wrapping Try/Option, etc, in ZIO (<a href="https://github.com/zio/zio-intellij/pull/83">#83</a>)</li>
        <li>Made "Suggest specific ZIO type alias" to only appear on ZIO type declarations (<a href="https://github.com/zio/zio-intellij/pull/86">#86</a>)</li>
        <li>Fixed an issue where the integrated test runner did not work in projects imported via BSP (<a href="https://github.com/zio/zio-intellij/pull/89">#89</a>)</li>
        <li>Fixed and improved various <code>.tap</code> suggestions (<a href="https://github.com/zio/zio-intellij/pull/93">#93</a>)</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
