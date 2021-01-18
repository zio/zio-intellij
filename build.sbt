lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2020.3.4" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

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
        Lots of bug fixes and improvements in this release, thanks a lot to everyone who reported issues!
        <ul>
          <li>A new template for creating ZIO projects with File -> New Project (<a href="https://github.com/zio/zio-intellij/pull/206">#206</a>)</li>
          <li>Correctly handling projects with multiple Scala versions present (<a href="https://github.com/zio/zio-intellij/pull/208">#208</a>)</li>
          <li>Adding support for the upcoming <code>MutableRunnableSpec</code> (<a href="https://github.com/zio/zio-intellij/pull/216">#216</a>)</li>
          <li>Miscellaneous bug fixes</li>
        </ul>
        ]]>"""
      )
    }
  )
