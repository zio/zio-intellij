package zio.intellij.testsupport

import com.intellij.execution.Location
import com.intellij.execution.actions.{ConfigurationContext, ConfigurationFromContext}
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationTypeUtil}
import com.intellij.openapi.module.Module
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiElement
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.runner.ScalaApplicationConfigurationProducer
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestConfigurationProducer
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestConfigurationProducer.CreateFromContextInfo
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestConfigurationProducer.CreateFromContextInfo._

final class ZTestRunConfigurationProducer extends AbstractTestConfigurationProducer[ZTestRunConfiguration] {

  override def getConfigurationFactory: ConfigurationFactory = {
    val configurationType = ConfigurationTypeUtil.findConfigurationType(classOf[ZTestConfigurationType])
    configurationType.confFactory
  }

  override protected def suitePaths: List[String] = ZSuitePaths

  override def shouldReplace(self: ConfigurationFromContext, other: ConfigurationFromContext): Boolean =
    other.isProducedBy(classOf[ScalaApplicationConfigurationProducer])

  override protected def configurationName(contextInfo: CreateFromContextInfo): String = contextInfo match {
    case AllInPackage(_, packageName) =>
      s"ZIO Tests in $packageName"
    case ClassWithTestName(testClass, testName) =>
      StringUtil.getShortName(testClass.qualifiedName) + testName.fold("")("::" + _)
  }

  override def getTestClassWithTestName(location: Location[_ <: PsiElement]): Option[ClassWithTestName] =
    location.getPsiElement match {
      case IsZioTestElement(td, tm) =>
        tm match {
          case Some(testName(name)) =>
            Some(ClassWithTestName(td, Some(name)))
          case _ =>
            Some(ClassWithTestName(td, None))
        }
      case _ => None
    }

  override def setupConfigurationFromContext(
    configuration: ZTestRunConfiguration,
    context: ConfigurationContext,
    sourceElement: Ref[PsiElement]
  ): Boolean = {
    val contextLocation = context.getLocation
    val contextModule   = contextLocation.getModule //context.getModule

    if (contextLocation == null || contextModule == null) false
    else if (sourceElement.isNull) false
    else if (!hasTestSuitesInModuleDependencies(contextModule)) false
    else {
      val maybeTuple = createConfigurationFromContextLocation(contextLocation)
      maybeTuple.fold(false) {
        case (testElement, confSettings) =>
          val config = confSettings.getConfiguration.asInstanceOf[ZTestRunConfiguration]
          // TODO: should we really check it for configuration (the one we should setup) and not for config (just created)?
          sourceElement.set(testElement)
          configuration.initFrom(config)
          true
      }
    }

  }

  private def hasTestSuitesInModuleDependencies(module: Module): Boolean = {
    val scope      = GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(module, true)
    val psiManager = ScalaPsiManager.instance(module.getProject)
    suitePaths.exists(psiManager.getCachedClass(scope, _).isDefined)
  }

}

object ZTestRunConfigurationProducer {
  // TODO this is copied from the Scala plugin
  // It should probably be created by the platform
  val instance = new ZTestRunConfigurationProducer
}
