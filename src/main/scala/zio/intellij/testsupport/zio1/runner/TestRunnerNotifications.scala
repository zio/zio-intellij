package zio.intellij.testsupport.zio1.runner

import com.intellij.notification._
import com.intellij.openapi.project.Project

import java.util.concurrent.ConcurrentHashMap
import javax.swing.Icon
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.ref.WeakReference

private[runner] object TestRunnerNotifications {

  private val notificationGroup =
    NotificationGroupManager.getInstance.getNotificationGroup("Test Runner Download")
  private val notificationErrorGroup =
    NotificationGroupManager.getInstance.getNotificationGroup("Test Runner Download Error")

  // do not display notification with same content several times
  private val messagesShown: mutable.Map[String, WeakReference[Notification]] =
    new ConcurrentHashMap[String, WeakReference[Notification]]().asScala

  private def displayNotification(
    message: String,
    notificationType: NotificationType,
    actions: Seq[NotificationAction] = Nil,
    icon: Option[Icon] = None
  )(implicit project: Project): Unit = {
    updateShownMessagesCache(message)
    if (messagesShown.contains(message)) return

    val notification: Notification =
      if (notificationType == NotificationType.INFORMATION) {
        notificationGroup.createNotification(message, notificationType)
      } else {
        notificationErrorGroup.createNotification(message, notificationType)
      }
    actions.foreach(notification.addAction)
    icon.foreach(notification.setIcon)
    notification.notify(project)

    messagesShown(message) = WeakReference(notification)
    notification.whenExpired(() => messagesShown.remove(message))
  }

  // notification.getBalloon can be null right after notification creation
  // so we can detect notification balloon close event only this way
  private def updateShownMessagesCache(message: String): Unit =
    messagesShown.get(message) match {
      case Some(WeakReference(notification)) =>
        val balloon = notification.getBalloon
        if (balloon == null || balloon.isDisposed) {
          messagesShown.remove(message)
        }
      case _ =>
        messagesShown.remove(message)
    }

  def displayInfo(
    message: String,
    actions: Seq[NotificationAction] = Nil,
    icon: Option[Icon] = None
  )(implicit project: Project = null): Unit =
    displayNotification(message, NotificationType.INFORMATION, actions, icon)

  def displayWarning(
    message: String,
    actions: Seq[NotificationAction] = Nil,
    icon: Option[Icon] = None
  )(implicit project: Project = null): Unit =
    displayNotification(message, NotificationType.WARNING, actions, icon)

  def displayError(
    message: String,
    actions: Seq[NotificationAction] = Nil,
    icon: Option[Icon] = None
  )(implicit project: Project = null): Unit =
    displayNotification(message, NotificationType.ERROR, actions, icon)
}
