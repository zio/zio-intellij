lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2020.3.5" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "203"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

scalacOptions += "-deprecation"

lazy val `zio-intellij` = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    scalaVersion := scala213,
    version := pluginVersion,
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala:2020.3.16".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        Welcome to another exciting release of the ZIO plugin for IntelliJ!<br/>
        Lots of bug fixes and improvements in this release, huge thanks to Yuriy Bogomolov, Mitsutaka Takeda, and other who contributed code and issues!
        <ul>
          <li>New refactoring: <code>*&gt; ZIO.succeed</code> -> <code>.as</code> (<a href="https://github.com/zio/zio-intellij/pull/225">#225</a>)</li>
          <li>New refactoring: <code>.map(...).flatten</code> -> <code>.flatMap</code> (<a href="https://github.com/zio/zio-intellij/pull/227">#227</a>)</li>
          <li>New refactoring: <code>.either.unit</code> -> <code>.ignore</code> (<a href="https://github.com/zio/zio-intellij/pull/228">#228</a>)</li>
          <li>Bug fix: Do not suggest <code>.unit</code> refactoring for chained expressions (<a href="https://github.com/zio/zio-intellij/pull/229">#229</a>)</li>
          <li>Bug fix: Failed to run tests due to invalid file path on Windows (<a href="https://github.com/zio/zio-intellij/pull/236">#236</a>)</li>
          <li>Feature: Do not automatically step into the ZIO Runtime during debugging</li>
          <li>Miscellaneous improvements</li>
        </ul>
        ]]>"""
      )
    }
  )
