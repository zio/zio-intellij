import org.jetbrains.sbtidea.{AutoJbr, JbrPlatform}

lazy val scala213           = "2.13.10"
lazy val scalaPluginVersion = "2023.1.9"
lazy val pluginVersion      = "2023.1.20" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold(".0")(v => s".$v")

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "231.6890.12"
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
        <b>What's new?</b>
        <ul>
          <li>Added some live templates for easier ZLayer and Service accessors creation (<a href="https://github.com/zio/zio-intellij/pull/392">#392</a>). Huge thanks to Mark Rudolph for the contribution!</li>
          <li>Added inspections for <code>.provide*</code> and <code>.inject*</code> (for zio-magic users) (<a href="https://github.com/zio/zio-intellij/pull/396">#396</a>, <a href="https://github.com/zio/zio-intellij/pull/399">#399</a>, <a href="https://github.com/zio/zio-intellij/pull/401">#401</a>). Huge thanks to Nikita Myazin for the contribution!</li>
          <li>Fixes for ZIO 2.0-specific <code>foreach*</code> suggestions (<a href="https://github.com/zio/zio-intellij/pull/394">#394</a>). Huge thanks to Marcel Kalai for the contribution!</li>
          <li>Many internal improvements for ZIO 2.0 support</li>
          <li>Lots of small bugfixes and yaks shaving</li>
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
      s"org.intellij.scala:$scalaPluginVersion".toPlugin
    ),
    (Test / scalacOptions) += "-Xmacro-settings:enable-expression-tracers"
  )
  
lazy val docs = project
  .in(file("zio-intellij-docs"))
  .settings(
    moduleName := "zio-intellij-docs",
    projectName                                := "ZIO Intellij",
    mainModuleName                             := (root / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady
  )
  .enablePlugins(WebsitePlugin)
