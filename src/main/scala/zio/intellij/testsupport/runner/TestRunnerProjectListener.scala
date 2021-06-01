package zio.intellij.testsupport.runner

import com.intellij.openapi.project.{Project, ProjectManagerListener}

private[runner] final class TestRunnerProjectListener extends ProjectManagerListener {

  override def projectOpened(project: Project): Unit =
    new TestRunnerProjectNotification(project).init()

  override def projectClosing(project: Project): Unit =
    TestRunnerResolveService.instance(project).clearCaches()
}
