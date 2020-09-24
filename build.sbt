lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.3.1.0"

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "203"

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
      "org.intellij.scala:2020.3.353".toPlugin
    ),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = s"""<![CDATA[
        <em>A huge thanks to Timur Aliberdov (<a href="https://github.com/timaliberdov">@timaliberdov</a>), as well as Nikita Myazin, and others who contributed to this release!</em>
        <ul>
        <li>Fixing an issue where certain refactorings erroneously deleted code blocks (<a href="https://github.com/zio/zio-intellij/pull/147">#147</a>)</li>
        <li>Added simplification for <code>ZIO.unless</code>, tweaked <code>ZIO.when</code> (<a href="https://github.com/zio/zio-intellij/pull/148">#148</a>)</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
