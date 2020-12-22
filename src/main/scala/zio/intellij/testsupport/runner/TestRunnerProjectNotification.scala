package zio.intellij.testsupport.runner

import com.intellij.ide.BrowserUtil
import com.intellij.notification._
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt}
import zio.intellij.ZioIcon
import zio.intellij.testsupport.runner.TestRunnerNotifications.{displayError, displayInfo}
import zio.intellij.utils.{ModuleSyntax, Version}

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
            Version.parseUnsafe("5.82.0"),
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
              override def actionPerformed(e: AnActionEvent, notification: Notification): Unit = {
                downloadTestRunner(notification)
              }
            },
            new NotificationAction("Report on GitHub (opens a browser)") {
              override def actionPerformed(e: AnActionEvent, notification: Notification): Unit = {
                ???
              }
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

  private val suggesterNotificationGroup: NotificationGroup =
    NotificationGroup.balloonGroup("ZIO Test Runner")
}
