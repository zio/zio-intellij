lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2021.1.12" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".1")(v => s".$v")

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "211"

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
      "org.intellij.scala:2021.1.22".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        <ul>
          <li>Fixing ZIO project wizard to select the correct version (<a href="https://github.com/zio/zio-intellij/pull/309">#309</a>)</li>
          <li>Removing <code>.bimap</code>, replacing with <code>.mapBoth</code> (<a href="https://github.com/zio/zio-intellij/pull/310">#310</a>)</li>
          <li>Added Create Test dialog for ZIO Test (you're welcome, Kit!) (<a href="https://github.com/zio/zio-intellij/pull/311">#311</a>)</li>
          <li>Added support for ZIO 2.0 and now using its built-in test runner! (<a href="https://github.com/zio/zio-intellij/pull/319">#319</a>)</li>
          <li>Miscellaneous bugfixes</li>
        </ul>
        ]]>"""
      )
    }
  )
