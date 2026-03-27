import org.jetbrains.sbtidea.IntelliJPlatform.IdeaCommunity
import org.jetbrains.sbtidea.IntellijPlugin
import org.jetbrains.sbtidea.Keys.*
import org.jetbrains.sbtidea.download.BuildInfo
import org.jetbrains.sbtidea.download.idea.IntellijVersionUtils
import sbt.*

object Versions {

  val scala213: String = "2.13.18"

  val intellijVersion: String      = "261.22158.277"
  val intellijHumanVersion: String = "2026.1" // just for `What's new?`

  val scalaPluginVersion: String = "2026.1.16"

  val minorVersion: String  = "0"
  val buildVersion: String  = sys.env.getOrElse("ZIO_INTELLIJ_BUILD_NUMBER", minorVersion)
  val pluginVersion: String = s"2026.1.3.$buildVersion"

  val IntellijTestFrameworkVersion: String = intellijVersion_ForManagedIntellijDependencies

  lazy val (
    intellijVersion_ForManagedIntellijDependencies,
    intellijRepository_ForManagedIntellijDependencies
  ) = detectIntellijArtifactVersionAndRepository(intellijVersion)

  private def detectIntellijArtifactVersionAndRepository(intellijVersion: String): (String, MavenRepository) = {
    val locationDescriptor =
      IntellijVersionUtils.detectArtifactLocation(BuildInfo(intellijVersion, IdeaCommunity), ".zip")
    val artifactVersion = locationDescriptor.artifactVersion
    val artifactUrl     = locationDescriptor.url
    (artifactVersion, locationDescriptor.repository)
  }

}

object Dependencies {

  val junit: Seq[ModuleID] =
    Seq(
      "junit"          % "junit"           % "4.13.2" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
      "org.opentest4j" % "opentest4j"      % "1.3.0"  % Test
    )

  val intellijTestFrameworkAll: Seq[ModuleID] = Seq(
    intellijTestFrameworkCore,
    intellijTestFrameworkCommon,
    intellijTestFramework,
    intellijJavaTestFrameworkShared,
    intellijJavaTestFrameworkBackend,
    intellijJavaTestFramework,
    intellijDebuggerTestFramework,
    intellijUastTestFramework
  ).map(d => d.notTransitive() % Test)

  val intellijPlugins: Seq[IntellijPlugin] =
    Seq(
      "com.intellij.java".toPlugin,
      s"org.intellij.scala:${Versions.scalaPluginVersion}".toPlugin
    )

  lazy val intellijTestFrameworkCore: ModuleID =
    "com.jetbrains.intellij.platform" % "test-framework-core" % Versions.IntellijTestFrameworkVersion
  lazy val intellijTestFrameworkCommon: ModuleID =
    "com.jetbrains.intellij.platform" % "test-framework-common" % Versions.IntellijTestFrameworkVersion
  lazy val intellijTestFramework: ModuleID =
    "com.jetbrains.intellij.platform" % "test-framework" % Versions.IntellijTestFrameworkVersion
  lazy val intellijJavaTestFrameworkShared: ModuleID =
    "com.jetbrains.intellij.java" % "java-test-framework-shared" % Versions.IntellijTestFrameworkVersion
  lazy val intellijJavaTestFrameworkBackend: ModuleID =
    "com.jetbrains.intellij.java" % "java-test-framework-backend" % Versions.IntellijTestFrameworkVersion
  lazy val intellijJavaTestFramework: ModuleID =
    "com.jetbrains.intellij.java" % "java-test-framework" % Versions.IntellijTestFrameworkVersion
  lazy val intellijDebuggerTestFramework: ModuleID =
    "com.jetbrains.intellij.platform" % "debugger-test-framework" % Versions.IntellijTestFrameworkVersion
  lazy val intellijUastTestFramework: ModuleID =
    "com.jetbrains.intellij.platform" % "uast-test-framework" % Versions.IntellijTestFrameworkVersion
}
