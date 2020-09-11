package zio.intellij.intentions

import com.intellij.openapi.editor.Editor
import org.jetbrains.plugins.scala.codeInsight.intention.types._
import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScBindingPattern, ScTypedPattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScUnderscoreSection
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import zio.intellij.inspections._

abstract class ZTypeAnnotationIntention extends AbstractTypeAnnotationIntention with ZIcon {
  final override def getText: String = getFamilyName

  override protected def descriptionStrategy: Strategy =
    ZStrategy {
      case (te, declaredType) => isOfClassFrom(declaredType, zioLikePackages) && shouldSuggest(te, declaredType)
      case _                  => false
    }

  override def invocationStrategy(maybeEditor: Option[Editor]): Strategy =
    ZStrategy {
      case (te, declaredType) if maybeEditor.isDefined => invoke(te, declaredType, maybeEditor.get)
      case _                                           => false
    }

  protected def invoke(te: ScTypeElement, declaredType: ScType, editor: Editor): Boolean

  protected def shouldSuggest(te: ScTypeElement, scType: ScType): Boolean = true
}

object ZStrategy {

  def apply(suggest: (ScTypeElement, ScType) => Boolean): Strategy =
    new Strategy {

      def canSuggest(te: ScTypeElement): Boolean =
        te.`type`().toOption match {
          case Some(declaredType) => suggest(te, declaredType)
          case _                  => false
        }

      override def functionWithType(function: ScFunctionDefinition, typeElement: ScTypeElement): Boolean =
        canSuggest(typeElement)

      override def valueWithType(value: ScPatternDefinition, typeElement: ScTypeElement): Boolean =
        canSuggest(typeElement)

      override def variableWithType(variable: ScVariableDefinition, typeElement: ScTypeElement): Boolean = false

      override def functionWithoutType(function: ScFunctionDefinition): Boolean = false

      override def valueWithoutType(value: ScPatternDefinition): Boolean = false

      override def variableWithoutType(variable: ScVariableDefinition): Boolean = false

      override def patternWithoutType(pattern: ScBindingPattern): Boolean = false

      override def wildcardPatternWithoutType(pattern: ScWildcardPattern): Boolean = false

      override def patternWithType(pattern: ScTypedPattern): Boolean = false

      override def parameterWithoutType(param: ScParameter): Boolean = false

      override def parameterWithType(param: ScParameter): Boolean = false

      override def underscoreSectionWithoutType(underscore: ScUnderscoreSection) = false

      override def underscoreSectionWithType(underscore: ScUnderscoreSection) = false
    }
}
