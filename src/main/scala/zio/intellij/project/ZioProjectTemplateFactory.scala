package zio.intellij.project

import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.platform.{ProjectTemplate, ProjectTemplatesFactory}
import org.jetbrains.plugins.scala.project.template.ScalaProjectTemplatesFactory
import zio.intellij.ZioIcon

import javax.swing.Icon

private[zio] class ZioProjectTemplateFactory extends ProjectTemplatesFactory {
  override def getGroups: Array[String] = Array(ScalaProjectTemplatesFactory.Group)

  override def getGroupIcon(group: String): Icon = ZioIcon

  override def createTemplates(group: String, context: WizardContext): Array[ProjectTemplate] =
    Array(new ZioProjectTemplate)
}
