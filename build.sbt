import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}

lazy val scala213           = "2.13.10"
lazy val scalaPluginVersion = "2022.3.16"
lazy val pluginVersion      = "2022.3.20" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "223"
ThisBuild / jbrInfo := AutoJbr(explicitPlatform = Some(JbrPlatform.osx_aarch64))

Global / intellijAttachSources := true

addCommandAlias("fmt", "scalafmtAll")
addCommandAlias("check", "scalafmtCheckAll")

(Global / javacOptions) := Seq("--release", "17")

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
        Preliminary support for ZIO 2.0.<br/>
        <b>Note:</b> While the ZIO 2.0 support is ongoing, there might be false ZIO 1.0 suggestions that do not work for ZIO 2.0. Please report any such instances to the plugin Github/Discord. Thanks!
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
      s"org.intellij.scala:$scalaPluginVersion".toPlugin
    ),
    (Test / scalacOptions) += "-Xmacro-settings:enable-expression-tracers"
  )
