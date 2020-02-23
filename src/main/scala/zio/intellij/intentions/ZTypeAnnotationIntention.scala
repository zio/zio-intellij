package zio.intellij.intentions

import com.intellij.openapi.editor.Editor
import org.jetbrains.plugins.scala.codeInsight.intention.types._
import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import zio.intellij.inspections.zioClasses


abstract class ZTypeAnnotationIntention extends AbstractTypeAnnotationIntention with ZIcon {
  final override def getText: String = getFamilyName

  override protected def descriptionStrategy: Strategy = ZStrategy {
    case (_, declaredType) => isOfClassFrom(declaredType, zioClasses)
    case _                 => false
  }

  override def invocationStrategy(maybeEditor: Option[Editor]): Strategy = ZStrategy {
    case (te, declaredType) if maybeEditor.isDefined => invoke(te, declaredType, maybeEditor.get)
    case _                                           => false
  }

  protected def invoke(te: ScTypeElement, declaredType: ScType, editor: Editor): Boolean
}

object ZStrategy {
  def apply(suggest: (ScTypeElement, ScType) => Boolean): Strategy = new Strategy {

    def canSuggest(te: ScTypeElement): Boolean = te.`type`().toOption match {
      case Some(declaredType) => suggest(te, declaredType)
      case _                  => false
    }

    override def functionWithType(function: ScFunctionDefinition, typeElement: ScTypeElement): Boolean =
      canSuggest(typeElement)

    override def valueWithType(value: ScPatternDefinition, typeElement: ScTypeElement): Boolean =
      canSuggest(typeElement)

    override def variableWithType(variable: ScVariableDefinition, typeElement: ScTypeElement): Boolean =
      canSuggest(typeElement)
  }
}
