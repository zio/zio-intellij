lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2020.3.5" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".1")(v => s".$v")

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
      "org.intellij.scala:2020.3.21".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        This is a small update, containing a fix for supporting not-so-SemVer ZIO 1.0.4-x updates<br/>
        Big thanks to Timur Aliberdov for fixing it!
        <ul>
          <li>Fixed an issue with the test runner failing to download on Scala 3 projects (<a href="https://github.com/zio/zio-intellij/pull/271">#271</a>)</li>
        </ul>
        ]]>"""
      )
    }
  )
