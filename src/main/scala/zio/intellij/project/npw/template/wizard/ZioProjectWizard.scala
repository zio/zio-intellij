package zio.intellij.project.npw.template.wizard

import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.ide.wizard._
import org.jetbrains.plugins.scala.util.ui.KotlinDslWrappers.StepChainOps
import org.jetbrains.sbt.project.template.wizard.ScalaNewProjectWizardMultiStep
import org.jetbrains.sbt.project.template.wizard.buildSystem.ZioNewProjectWizardStep
import zio.intellij.ZioIcon

import javax.swing.Icon
import scala.annotation.nowarn

object ZioProjectWizard extends GeneratorNewProjectWizard {

  override def isEnabled: Boolean = true

  override def getIcon: Icon = ZioIcon

  override def getId: String = "ZioProjectWizard"

  override def getName: String = "ZIO"

  @nowarn("cat=deprecation")
  override def createStep(wizardContext: WizardContext): NewProjectWizardStep =
    new RootNewProjectWizardStep(wizardContext)
      .nextStep(new NewProjectWizardBaseStep(_))
      .nextStep(new NewProjectWizardLanguageStep(_, "Scala")) // Required to trick the wizard into thinking it's a Scala project (provides `LanguageNewProjectWizardData`)
      .nextStep(parent => new ZioNewProjectWizardStep(new ScalaNewProjectWizardMultiStep(parent)))

}
