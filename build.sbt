import Versions.*
import org.jetbrains.sbtidea.packaging.PackagingMethod

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild      := intellijVersion

ThisBuild / autoRemoveOldCachedIntelliJSDK := true
ThisBuild / autoRemoveOldCachedDownloads   := true

Global / intellijAttachSources := true

addCommandAlias("fmt", "scalafmtAll")
addCommandAlias("check", "scalafmtCheckAll")

ThisBuild / javacOptions := Seq("--release", "25")

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
  "-Wconf:msg=legacy-binding:s",
  "-Ytasty-reader"
)

lazy val zio2TestRunner =
  Project("zio2-test-runner", file("zio2-test-runner"))
    .settings(
      name         := "zio2-test-runner",
      scalaVersion := scala213,
      // This runner is executed inside the *user's* project JVM, not the IDE, so it must target an old enough
      // bytecode version to load on whatever JDK their project uses.
      Compile / javacOptions := Seq("--release", "8"),
      packageMethod          := PackagingMethod.Standalone()
    )

lazy val root =
  newProject("zio-intellij", file("."))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      patchPluginXml := pluginXmlOptions { xml =>
        xml.version = version.value
        xml.changeNotes = sys.env.getOrElse(
          s"ZIO_INTELLIJ_CHANGE_NOTES",
          s"""<![CDATA[
        <b>What's new?</b>
        <ul>
          <li>IntelliJ IDEA $intellijHumanVersion support!</li>
          <li>Plugin now supports running all ZIO tests in a package</li>
          <li>BSP environment support for ZIO Test run/debug configuration</li>
        </ul>
        <b>Note:</b> The ZIO project wizard is temporarily disabled due to incompatibility issues.
        ]]>"""
        )
      },
      // zio2-test-runner.jar is packaged as a standalone JAR (lib/zio2-test-runner.jar in the plugin distribution)
      // using PackagingMethod.Standalone, mirroring the pattern used by the Scala plugin for other test runners
      packageAdditionalProjects += zio2TestRunner
    )
    .dependsOn(macros, scalaPluginTestkit % "test->test")

// Vendored copy of the IntelliJ Scala plugin test framework — see scala-plugin-testkit/ORIGIN.md.
lazy val scalaPluginTestkit =
  newProject("scala-plugin-testkit", file("scala-plugin-testkit"))
    .enablePlugins(SbtIdeaPlugin)

lazy val macros =
  newProject("macros", file("macros"))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scala213 intransitive ()
      )
    )

def newProject(projectName: String, base: File): Project =
  Project(projectName, base)
    .settings(
      name         := projectName,
      scalaVersion := scala213,
      version      := pluginVersion,
      resolvers += Versions.intellijRepository_ForManagedIntellijDependencies,
      libraryDependencies ++= Dependencies.junit,
      libraryDependencies ++= Dependencies.intellijTestFrameworkAll,
      intellijPlugins := Dependencies.intellijPlugins,
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
      (Test / scalacOptions) += "-Xmacro-settings:enable-expression-tracers"
    )
