lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2021.1.10" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

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
      "org.intellij.scala:2021.1.20".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        <ul>
          <li>Detecting incorrect usage of <code>class</code> instead of <code>object</code> in ZIO specs (<a href="https://github.com/zio/zio-intellij/pull/279">#279</a>)</li>
          <li>Restricting the catchAll-tap refactoring to only when using <code>ZIO.fail(a)</code> where <code>a</code> is the parameter to <code>catchAll</code> (<a href="https://github.com/zio/zio-intellij/pull/285">#285</a>)</li>
          <li><code>.serviceWith</code> support (<a href="https://github.com/zio/zio-intellij/pull/287">#287</a>)</li>
          <li>Miscellaneous improvements and bugfixes</li>
        </ul>
        ]]>"""
      )
    }
  )
