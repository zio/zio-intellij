package zio.intellij.testsupport

import com.intellij.execution.Location
import com.intellij.execution.actions.{ConfigurationContext, ConfigurationFromContext}
import com.intellij.execution.application.ApplicationConfigurationProducer
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationTypeUtil}
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.{PsiDirectory, PsiElement, PsiPackage}
import org.jetbrains.plugins.scala.runner.ScalaApplicationConfigurationProducer
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestConfigurationProducer
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestConfigurationProducer.CreateFromContextInfo
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestConfigurationProducer.CreateFromContextInfo._
import zio.intellij.testsupport.ZTestFramework.{ZIO1SpecFQN, ZIO2SpecFQN}

final class ZTestRunConfigurationProducer extends AbstractTestConfigurationProducer[ZTestRunConfiguration] {

  override def getConfigurationFactory: ConfigurationFactory = {
    val configurationType = ConfigurationTypeUtil.findConfigurationType(classOf[ZTestConfigurationType])
    configurationType.confFactory
  }

  override protected def suitePaths: List[String] = List(ZIO1SpecFQN, ZIO2SpecFQN)

  override def shouldReplace(self: ConfigurationFromContext, other: ConfigurationFromContext): Boolean =
    other.isProducedBy(classOf[ScalaApplicationConfigurationProducer]) ||
      other.isProducedBy(classOf[ApplicationConfigurationProducer])

  override def setupConfigurationFromContext(
    configuration: ZTestRunConfiguration,
    context: ConfigurationContext,
    sourceElement: Ref[PsiElement]
  ): Boolean =
    Option(context.getLocation).fold(false) { location =>
      val psiElement = location.getPsiElement
      psiElement match {
        case _: PsiDirectory | _: PsiPackage => false // TODO: disabled until the test runner supports multiple specs
        case _                               => super.setupConfigurationFromContext(configuration, context, sourceElement)
      }
    }

  override protected def configurationName(contextInfo: CreateFromContextInfo): String =
    contextInfo match {
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
}
