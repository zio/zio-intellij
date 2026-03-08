package zio.intellij.testsupport.zio1.runner

import com.intellij.ide.BrowserUtil
import com.intellij.notification._
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.project.Project
import zio.intellij.testsupport.zio1.runner.TestRunnerNotifications.{displayError, displayInfo}
import zio.intellij.testsupport.zio1.runner.TestRunnerResolveService.ResolveException
import zio.intellij.utils.ProjectSyntax
import zio.intellij.ZioIcon

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private[testsupport] final class TestRunnerProjectNotification(private val project: Project) {
  def init(): Unit =
    if (shouldSuggestTestRunner(project)) {
      createNotification.notify(project)
    }

  private def shouldSuggestTestRunner(project: Project, downloadIfMissing: Boolean = false): Boolean = {
    val zioVersions = project.versions.map(_._1)
    // Sometimes there are mixed ZIO versions in the project.
    // Only offer downloading the test runner if all projects use ZIO 1.0
    if (zioVersions.forall(_.requiresTestRunner)) {
      project.versions.foldLeft(false) {
        case (flag, (version, scalaVersion)) =>
          flag | (version.requiresTestRunner && TestRunnerResolveService
            .instance(project)
            .resolve(version, scalaVersion, downloadIfMissing)
            .toOption
            .isEmpty)

      }
    } else false
  }

  private def downloadTestRunner(notification: Notification): Unit = {
    val tasks = project.versions.map {
      case (version, scalaVersion) =>
        TestRunnerResolveService
          .instance(project)
          .resolveAsync(
            version,
            scalaVersion,
            project
          )
    }
    Future.sequence(tasks).foreach { results =>
      if (results.forall(_.isRight)) displayInfo("ZIO Test runner was downloaded successfully!")
      else {
        displayError(
          "Unable to download one or more required files. Please try again, or report this issue on GitHub, " +
            "if the problem persists.",
          Seq(),
          icon = Some(ZioIcon)
        )
        val errors = results.collect { case Left(error) => error }
        throw new ResolveException(errors)
      }
    }
    notification.expire()
  }

  private def createNotification: Notification =
    suggesterNotificationGroup
      .createNotification("Enable the integrated ZIO Test runner", NotificationType.INFORMATION)
      .addAction(new NotificationAction("Download ZIO Test runner") {
        override def actionPerformed(e: AnActionEvent, notification: Notification): Unit =
          downloadTestRunner(notification)
      })
      .addAction(new NotificationAction("Learn more...") {
        override def actionPerformed(e: AnActionEvent, notification: Notification): Unit =
          BrowserUtil.open("https://plugins.jetbrains.com/plugin/13820-zio-for-intellij/zio-test-runner")
      })
      .setIcon(ZioIcon)

  private val suggesterNotificationGroup: NotificationGroup =
    NotificationGroupManager.getInstance.getNotificationGroup("Test Runner Download")
}
