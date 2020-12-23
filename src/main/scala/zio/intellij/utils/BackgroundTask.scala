package zio.intellij.utils

import com.intellij.openapi.progress.{PerformInBackgroundOption, ProcessCanceledException, ProgressIndicator, Task}
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.NonNls

import scala.concurrent.{Future, Promise}
import scala.util.Try

object BackgroundTask {
  def apply[A](project: Project, @NonNls title: String, @NonNls cancelText: String)(
    f: ProgressIndicator => A
  ): Future[A] = {
    val p = Promise[A]()

    new Task.Backgroundable(project, title, true, PerformInBackgroundOption.ALWAYS_BACKGROUND) {

      override def run(indicator: ProgressIndicator): Unit =
        p.complete(Try(f(indicator)))

      override def onThrowable(error: Throwable): Unit =
        p.failure(error)

      override def onCancel(): Unit =
        p.tryFailure(new ProcessCanceledException())

    }.setCancelText(cancelText).queue()

    p.future
  }
}
