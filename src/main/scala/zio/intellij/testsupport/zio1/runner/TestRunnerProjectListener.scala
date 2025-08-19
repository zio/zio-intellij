package zio.intellij.testsupport.zio1.runner

import com.intellij.openapi.project.{Project, ProjectManagerListener}
import com.intellij.openapi.startup.StartupActivity

private[testsupport] final class TestRunnerProjectListener extends ProjectManagerListener with StartupActivity {
  override def runActivity(project: Project): Unit =
    new TestRunnerProjectNotification(project).init()

  override def projectClosing(project: Project): Unit =
    TestRunnerResolveService.instance(project).clearCaches()
}
