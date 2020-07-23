lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.1.0.9"

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
      "org.intellij.scala:2020.1.41".toPlugin
    ),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = s"""<![CDATA[
        Welcome to another release packed with incredible new features!
        <em>A huge thanks to Timur Aliberdov (<a href="https://github.com/timaliberdov">@timaliberdov</a>), as well as Simon Popugaev, Andrés González and others who contributed to this release!</em>
        <ul>
        <li>A new Dump ZIO Fibers debug action (<a href="https://github.com/zio/zio-intellij/pull/117">#117</a>)</li>
        <li>Add support for <code>@mockable</code> macro (<a href="https://github.com/zio/zio-intellij/pull/120">#120</a>)</li>
        <li>Suggest <code>.toLayer/</code></code>.toLayerMany</code> (<a href="https://github.com/zio/zio-intellij/pull/116">#116</a>)</li>
        <li>Fix <code>@accessible</code> macro generation (<a href="https://github.com/zio/zio-intellij/pull/43">#43</a>)</li>
        <li>Add <code>.flatMap</code> to <code>.zipRight</code> inspection (<a href="https://github.com/zio/zio-intellij/pull/114">#114</a>)</li>
        <li>Fix a wrong suggestion for <code>.fromOption</code> (<a href="https://github.com/zio/zio-intellij/pull/122">#122</a>)</li>
        <li>Fix a crash in "Create new Test" when used with ZIO Tests (<a href="https://github.com/zio/zio-intellij/pull/125">#125</a>)</li>
        <li>Depending on the latest Scala plugin for compatibility</li>
        <li>Additional bug fixes and tweaks</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
