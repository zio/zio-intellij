lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.1.0.8"

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
      "org.intellij.scala:2020.1.36".toPlugin
    ),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = s"""<![CDATA[
        Welcome to another release packed with incredible new features!
        <em>A huge thanks to Timur Aliberdov (<a href="https://github.com/timaliberdov">@timaliberdov</a>) for contributing many features to this release!</em>
        <ul>
        <li>Adding <code>.exitCode</code> refactoring for ZIO RC20 (<a href="https://github.com/zio/zio-intellij/pull/105">#105</a>)</li>
        <li><li>Suggest <code>ZIO.none</code>, <code>ZIO.some</code>, <code>ZIO.left</code> and <code>ZIO.right</code> (<a href="https://github.com/zio/zio-intellij/pull/109">#109</a> - by @timaliberdov)</li>
        <li>Indicate (with an icon) code that's explicitly being forked (<a href="https://github.com/zio/zio-intellij/pull/110">#110</a> - by @timaliberdov)</li>
        <li>Convert <code>*></code> chain to for-comprehension and vice versa (<a href="https://github.com/zio/zio-intellij/pull/111">#111</a> - by @timaliberdov)</li>
        <li>Depending on the latest Scala plugin for compatibility</li>
        <li>Additional bug fixes and tweaks</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
