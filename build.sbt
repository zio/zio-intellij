lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.2.1.0"

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "202"

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
      "org.intellij.scala:2020.2.25".toPlugin
    ),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = s"""<![CDATA[
        Welcome to another release packed with incredible new features!
        <em>A huge thanks to Timur Aliberdov (<a href="https://github.com/timaliberdov">@timaliberdov</a>), as well as Nikita Myazin, and others who contributed to this release!</em>
        <ul>
        <li>Add simplification from <code>ZIO.foreach</code> to <code>ZIO.foreach_</code> (<a href="https://github.com/zio/zio-intellij/pull/135">#135</a>)</li>
        <li>Add <code>ZManaged</code> and poly services support to <code>@accessible</code> (<a href="https://github.com/zio/zio-intellij/pull/136">#136</a>)</li>
        <li>Detect if guards mistakenly used in for-comprehension on ZIO effect (<a href="https://github.com/zio/zio-intellij/pull/137">#137</a>)</li>
        <li>Making ZIO test method detection work for specs other than <code>DefaultRunnableSpec</code> (<a href="https://github.com/zio/zio-intellij/pull/138">#138</a>)</li>
        <li>Fixes incorrect test method detection (<a href="https://github.com/zio/zio-intellij/pull/139">#139</a>)</li>
        <li>Fix NPE when processing an incomplete test/suite string literal definition (<a href="https://github.com/zio/zio-intellij/pull/141">#141</a>)</li>
        <li>Fixes erroneous suggestions to non-effect types, such as <code>Chunk</code> (<a href="https://github.com/zio/zio-intellij/pull/142">#142</a>)</li>
        <li>Fixed not recognizing methods with default implementation when using <code>@accessible</code> (<a href="https://github.com/zio/zio-intellij/pull/144">#144</a>)</li>
        <li>Depending on the latest Scala plugin for compatibility</li>
        <li>Additional bug fixes and tweaks</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
