import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}

lazy val scala213           = "2.13.14"
lazy val scalaPluginVersion = "2024.2.28"
lazy val minorVersion       = "0"
lazy val buildVersion       = sys.env.getOrElse("ZIO_INTELLIJ_BUILD_NUMBER", minorVersion)
lazy val pluginVersion      = s"2024.2.35.$buildVersion"

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "242.22855.74"
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
  "-language:existentials",
  "-Wconf:msg=legacy-binding:s"
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
        <b>What's new?</b>
        <ul>
          <li>IntelliJ IDEA 2024.2 support!</li>
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
    libraryDependencies ++= Seq(
      "com.novocode"      % "junit-interface"   % "0.11"   % Test,
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.2" % Test
    ),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      s"org.intellij.scala:$scalaPluginVersion".toPlugin
    ),
    (Test / scalacOptions) += "-Xmacro-settings:enable-expression-tracers"
  )
