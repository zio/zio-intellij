package zio.intellij.testsupport.runner

import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.ide.{BrowserUtil, IdeBundle}
import com.intellij.notification._
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.application.ex.ApplicationInfoEx
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.SystemInfo
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt}
import zio.intellij.ZioIcon
import zio.intellij.testsupport.runner.TestRunnerNotifications.{displayError, displayInfo}
import zio.intellij.testsupport.runner.TestRunnerResolveService.{ResolveError, ResolveResult}
import zio.intellij.utils.{ModuleSyntax, ScalaVersionHack, StringBuilderSyntax, Version}

import java.net.URLEncoder
import javax.swing.event.HyperlinkEvent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private[runner] final class TestRunnerProjectNotification(private val project: Project) {
  def init(): Unit =
    if (shouldSuggestTestRunner(project)) {
      createNotification.notify(project)
    }

  private def versions(project: Project) = {
    val sourceModules = project.modulesWithScala.filter(_.isSourceModule).toList

    sourceModules.flatMap { m =>
      // First ZIO Test runner release: RC18-2
      // Do not try to download test runner for ZIO versions without runner release
      val zioVersion = m.zioVersion.filter(_ >= Version.ZIO.`RC18-2`)
      zioVersion zip m.scalaVersion
    }.distinct
  }

  private def shouldSuggestTestRunner(project: Project, downloadIfMissing: Boolean = false): Boolean =
    versions(project).foldLeft(false) {
      case (flag, (version, scalaVersion)) =>
        flag | TestRunnerResolveService.instance
          .resolve(version, scalaVersion, downloadIfMissing)
          .toOption
          .isEmpty
    }

  //noinspection HardCodedStringLiteral
  private def href(ref: String, text: String): String = s"""<a href="$ref">$text</a>"""
  @NonNls private val Nbsp                            = "&nbsp;"

  private val listener: NotificationListener = (notification: Notification, link: HyperlinkEvent) => {
    link.getDescription match {
      case "download" => downloadTestRunner(notification)
      case "learn_more" =>
        BrowserUtil.open("https://plugins.jetbrains.com/plugin/13820-zio-for-intellij/zio-test-runner")
      case _ =>
    }
  }

  private def downloadTestRunner(notification: Notification): Unit = {
    val tasks = versions(project).map {
      case (version, scalaVersion) =>
        TestRunnerResolveService.instance
          .resolveAsync(
            version,
            scalaVersion,
            project
          )
    }

    Future.sequence(tasks).foreach { results =>
      if (results.forall(_.isRight)) displayInfo("ZIO Test runner was downloaded successfully!")
      else
        displayError(
          "Unable to download one or more required files. Please try again, or report this issue on GitHub, if the problem persists.",
          Seq(
            new NotificationAction("Try again") {
              override def actionPerformed(e: AnActionEvent, notification: Notification): Unit =
                downloadTestRunner(notification)
            },
            new NotificationAction("Report on GitHub (opens a browser)") {
              override def actionPerformed(e: AnActionEvent, notification: Notification): Unit =
                reportErrorOnGithub(results, notification)
            }
          ),
          icon = Some(ZioIcon)
        )
    }
    notification.expire()
  }

  private def createNotification: Notification =
    suggesterNotificationGroup
      .createNotification(
        "Enable the integrated ZIO Test runner",
        href("download", "Download ZIO Test runner") + Nbsp * 6 +
          href("learn_more", "Learn more..."),
        NotificationType.INFORMATION,
        listener
      )
      .setIcon(ZioIcon)

  private def reportErrorOnGithub(results: List[ResolveResult], notification: Notification) = {
    val errors = results.collect {
      case Left(error) => error
    }

    // (ノಠ益ಠ)ノ彡┻━┻
    val sb = new StringBuilder()
    sb.appendLine("The following error(s) occurred while downloading the ZIO Test runner files:").appendLine
    sb.appendLine("```")
    sb.appendLine(errors.map {
      case ResolveError.NotFound(version, scalaVersion) =>
        s"Not found: zio-test-intellij_${scalaVersion.versionStr}:$version"
      case ResolveError.DownloadInProgress(version, scalaVersion) =>
        s"Download in progress: zio-test-intellij_${scalaVersion.versionStr}:$version"
      case ResolveError.DownloadError(version, scalaVersion, cause) =>
        s"""Download error: zio-test-intellij_${scalaVersion.versionStr}:$version"
           |Cause:
           |${cause.toString}""".stripMargin
      case ResolveError.UnknownError(version, scalaVersion, cause) =>
        s"""Unknown error: zio-test-intellij_${scalaVersion.versionStr}:$version"
           |Cause:
           |${cause.toString}""".stripMargin
    }.mkString("---"))
    sb.appendLine("```")
    sb.appendLine("### Additional information:")
    sb.appendLine("<details>")
    sb.appendLine
    sb.appendLine(s"ZIO plugin version: ${pluginVersion("zio.intellij.zio-intellij").getOrElse("unknown")}")
    sb.appendLine(s"Scala plugin version: ${pluginVersion("org.intellij.scala").getOrElse("unknown")}")
    sb.appendLine(s"ZIO version(s): ${versions(project).map(_._1).mkString(", ")}")
    sb.appendLine(s"Scala version(s): ${versions(project).map(_._2.minor).mkString(", ")}")
    val appInfo = ApplicationInfoEx.getInstanceEx
    sb.appendLine(s"IntelliJ version: ${appInfo.getFullApplicationName}")
    sb.appendLine(s"${IdeBundle.message("about.box.build.number", appInfo.getBuild.asString)}")
    sb.append("Java version: ")
    val properties = System.getProperties
    sb.appendLine(properties.getProperty("java.runtime.version", properties.getProperty("java.version", "unknown")))
    sb.append("Operating System: ")
    sb.appendLine(
      s"${properties.getProperty("os.arch", "")} ${SystemInfo.OS_NAME + " (" + SystemInfo.OS_VERSION + ", " + SystemInfo.OS_ARCH + ")"}"
    )
    sb.append("JVM version: ")
    sb.appendLine(
      s"${properties.getProperty("java.vm.name", "unknown")} ${properties.getProperty("java.vendor", "unknown")}"
    )
    sb.appendLine("</details>")

    val body = URLEncoder.encode(sb.result(), "UTF-8")

    try BrowserUtil.browse(
      s"https://github.com/zio/zio-intellij/issues/new?title=Problem+downloading+the+test+runner&labels=test-runner&body=$body"
    )
    finally notification.expire()
  }

  private def pluginVersion(id: String): Option[String] =
    PluginManagerCore.getPlugins
      .find(_.getPluginId.getIdString == id)
      .map(_.getVersion)

  private val suggesterNotificationGroup: NotificationGroup =
    NotificationGroupManager.getInstance.getNotificationGroup("Test Runner Download")
}
