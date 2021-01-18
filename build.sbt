lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2020.3.4" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "203"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)
// See https://github.com/JetBrains/sbt-idea-plugin/issues/76 for
// why this contrived sequence of actions exists ...
addCommandAlias(
  "packagePlugin",
  Seq(
    "project root",
    "packageArtifact",
    "doPatchPluginXml",
    "packageArtifactZip"
  ).mkString(";", ";", "")
)

scalacOptions += "-deprecation"

lazy val root = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    name := "zio-intellij",
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
        <ul>
          <li>Fixed an issue with <code>NeedsEnv</code> detection in certain cases (<a href="https://github.com/zio/zio-intellij/pull/200">#200</a>)</li>
          <li>Performance and stability fixes</li>
        </ul>
        ]]>"""
      )
    }
  )
