lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.1.0.5"

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
        Lots of new features and bugfixes in this release!
        <ul>
        <li>Detecting ZIO effects being used as statements (<a href="https://github.com/zio/zio-intellij/pull/60">#60</a>)</li>
        <li>Simplify peeking at errors with <code>.tap</code>/<code>.tapError</code>/</code>.tapBoth</code> (<a href="https://github.com/zio/zio-intellij/pull/65">#65</a>)</li>
        <li>Improved suggesting a more specific ZIO type alias: now handles all ZIO types (<a href="https://github.com/zio/zio-intellij/pull/66">#66</a>)</li>
        <li>A suggestion to use <code>.delay</code> instead of <code>ZIO.sleep</code> (<a href="https://github.com/zio/zio-intellij/pull/67">#67</a>)</li>
        <li>Warning about mistakingly wrapping the result in <code>yield</code> with a ZIO effect (<a href="https://github.com/zio/zio-intellij/pull/68">#68</a> - thanks to @timaliberdov)</li>
        <li>Detecting a possibly erroneous wrapping in ZIO of Try, Option, Either, and Future values (<a href="https://github.com/zio/zio-intellij/pull/70">#70</a>)</li>
        <li>Detecting the use of <code>Nothing</code> in the contravariant position of ZIO types (<a href="https://github.com/zio/zio-intellij/pull/76">#76</a>)</li>
        <li>Miscellaneous bug fixes</li>
        <ul>
        ]]>"""
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
