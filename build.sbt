lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2021.1.7" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

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
        Welcome to another exciting release of the ZIO plugin for IntelliJ!<br/>
        Lots of bug fixes and improvements in this release, thanks a lot to everyone who reported issues and contributed features!
        <ul>
          <li>Wrap infix expressions and for comprehensions in when/unless simplification (<a href="https://github.com/zio/zio-intellij/pull/263">#263</a>)</li>
          <li>Prevent test runner prompt in projects with unsupported ZIO versions (<a href="https://github.com/zio/zio-intellij/pull/264">#264</a>)</li>
          <li>Macros: Generate accessors for "flat" (ZIO 2.0) service (<a href="https://github.com/zio/zio-intellij/pull/267">#267</a>)</li>
          <li>Miscellaneous bug fixes</li>
        </ul>
        ]]>"""
      )
    }
  )
