package zio.intellij.testsupport.zio1.runner

import com.intellij.openapi.project.{Project, ProjectManagerListener}

private[testsupport] final class TestRunnerProjectListener extends ProjectManagerListener {

  override def projectOpened(project: Project): Unit =
    new TestRunnerProjectNotification(project).init()

  override def projectClosing(project: Project): Unit =
    TestRunnerResolveService.instance(project).clearCaches()
}
