lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2021.3.13" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "213"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

ThisBuild / scalacOptions ++= Seq(
  "-explaintypes",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xlint:serial",
  "-Ymacro-annotations",
  "-Xfatal-warnings",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-language:existentials"
)

lazy val root =
  newProject("zio-intellij", file("."))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      patchPluginXml := pluginXmlOptions { xml =>
        xml.version = version.value
        xml.changeNotes = sys.env.getOrElse(
          "ZIO_INTELLIJ_CHANGE_NOTES",
          s"""<![CDATA[
        <ul>
          <li>A new refactoring, converting <code>assert</code> into <code>assertTrue</code> (<a href="https://github.com/zio/zio-intellij/pull/328">#328</a>)</li>
          <li>Miscellaneous bugfixes and improvements</li>
        </ul>
        ]]>"""
        )
      }
    )
    .dependsOn(macros)

lazy val macros = newProject("macros", file("macros"))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scala213 intransitive ()
    )
  )

def newProject(projectName: String, base: File): Project =
  Project(projectName, base).settings(
    name := projectName,
    scalaVersion := scala213,
    version := pluginVersion,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala:2021.3.8".toPlugin
    ),
    (Test / scalacOptions) += "-Xmacro-settings:enable-expression-tracers"
  )
