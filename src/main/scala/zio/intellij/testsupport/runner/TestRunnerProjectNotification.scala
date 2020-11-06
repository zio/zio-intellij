package zio.intellij.testsupport.runner

import com.intellij.ide.BrowserUtil
import com.intellij.notification.{Notification, NotificationGroup, NotificationListener, NotificationType}
import com.intellij.openapi.project.Project
import javax.swing.event.HyperlinkEvent
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt}
import zio.intellij.ZioIcon
import zio.intellij.testsupport.runner.TestRunnerNotifications.displayInfo
import zio.intellij.utils.{ModuleSyntax, TraverseAtHome}

private[runner] final class TestRunnerProjectNotification(private val project: Project) {
  def init(): Unit =
    if (!findTestRunner(project)) {
      createNotification.notify(project)
    }

  private def versions(project: Project) = {
    val sourceModules = project.modulesWithScala.filter(_.isSourceModule).toList

    sourceModules
      // but we already have traverse at home...
      // (converts a pair of options to option of pair)
      .traverse(m => (m.zioVersion zip m.scalaVersion).headOption)
      .flatMap(_.headOption)
  }

  private def findTestRunner(project: Project, downloadIfMissing: Boolean = false): Boolean =
    versions(project).fold(false) {
      case (version, scalaVersion) =>
        TestRunnerResolveService.instance
          .resolve(version, scalaVersion, downloadIfMissing)
          .toOption
          .isDefined
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
                  case Right(_) => displayInfo("ZIO Test runner was downloaded successfully!")
                  case _        => // relying on error reporting in resolve method
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
