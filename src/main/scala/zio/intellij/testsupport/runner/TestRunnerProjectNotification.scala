package zio.intellij.testsupport.runner

import com.intellij.ide.BrowserUtil
import com.intellij.notification.{Notification, NotificationGroup, NotificationListener, NotificationType}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt}
import zio.intellij.ZioIcon
import zio.intellij.utils.ModuleSyntax

import javax.swing.event.HyperlinkEvent

private[runner] final class TestRunnerProjectNotification(private val project: Project) {
  def init(): Unit =
    if (shouldSuggestTestRunner(project)) {
      createNotification.notify(project)
    }

  private def versions(project: Project) = {
    val sourceModules = project.modulesWithScala.filter(_.isSourceModule).toList

    sourceModules
      .flatMap(m => m.zioVersion zip m.scalaVersion)
      .distinct
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
      case "download" =>
        versions(project).foreach {
          case (version, scalaVersion) =>
            TestRunnerResolveService.instance
              .resolveAsync(
                version,
                scalaVersion,
                project,
                onResolved = {
                  case Right(_) =>
                    val logger = Logger.getInstance("ZIO plugin for IntelliJ")
                    logger.info(s"ZIO Test runner for $version (${scalaVersion.major}) was downloaded successfully!")
                  case _ => // relying on error reporting in resolve method
                }
              )
        }
        notification.expire()
      case "learn_more" =>
        BrowserUtil.open("https://plugins.jetbrains.com/plugin/13820-zio-for-intellij/zio-test-runner")
      case _ =>
    }
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

  private val suggesterNotificationGroup: NotificationGroup =
    NotificationGroup.balloonGroup("ZIO Test Runner")
}
