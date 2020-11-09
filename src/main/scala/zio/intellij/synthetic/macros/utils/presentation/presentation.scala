package zio.intellij.synthetic.macros.utils

import org.apache.commons.lang.StringEscapeUtils
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScTypeUtil

// TODO: use org.jetbrains.plugins.scala.lang.psi.types.api.presentation classes introduced in scala plugin 2020.2.
//  Taken from different versions of Scala plugin source code for compatibility.
package object presentation {

  private[macros] def defaultPresentationStringForScalaType(scType: ScType): String =
    scType.presentableText(TypePresentationContext.emptyContext)

  private[macros] def presentationStringForScalaParameters(parameters: ScParameters): String =
    parameters.clauses.map(presentationStringForScalaParameterClause).mkString

  private[macros] def presentationStringForScalaParameterClause(clause: ScParameterClause): String = {
    val buffer = new StringBuilder("(")
    if (clause.isImplicit)
      buffer.append("implicit ")
    buffer.append(clause.parameters.map(presentationStringForScalaParameter).mkString(", "))
    buffer.append(")")
    buffer.result
  }

  private[macros] def presentationStringForScalaParameter(parameter: ScParameter): String = {
    val buffer = new StringBuilder
    buffer.append(parameterAnnotations(parameter))
    buffer.append(escape(parameter.name))
    buffer.append(parameterTypeAnnotations(parameter))
    buffer.result
  }

  private[macros] def presentationStringForScalaTypeParameters(
    tpClause: ScTypeParamClause,
    showVariance: Boolean
  ): String = presentationStringForScalaTypeParameters(tpClause.typeParameters, showVariance)

  private[macros] def presentationStringForScalaTypeParameters(
    tParams: Seq[ScTypeParam],
    showVariance: Boolean
  ): String = tParams.map(presentationStringForScalaTypeParam(_, showVariance)).mkString("[", ", ", "]")

  private[macros] def presentationStringForScalaTypeParam(param: ScTypeParam, showVariance: Boolean): String = {
    val initialText = if (showVariance) {
      if (param.isContravariant) "-"
      else if (param.isCovariant) "+"
      else ""
    } else ""

    val buffer = new StringBuilder(initialText)
    buffer.append(param.name)

    val stdTypes = param.projectContext.stdTypes
    param.lowerBound.foreach {
      case stdTypes.Nothing =>
      case tp: ScType =>
        buffer.append(" >: ")
        buffer.append(defaultPresentationStringForScalaType(tp))
    }
    param.upperBound.foreach {
      case stdTypes.Any =>
      case tp: ScType =>
        buffer.append(" <: ")
        buffer.append(defaultPresentationStringForScalaType(tp))
    }
    param.viewBound.foreach { tp =>
      buffer.append(" <% ")
      buffer.append(defaultPresentationStringForScalaType(tp))
    }
    param.contextBound.foreach { tp =>
      buffer.append(" : ")
      buffer.append(defaultPresentationStringForScalaType(ScTypeUtil.stripTypeArgs(tp)))
    }
    buffer.result
  }

  private def renderAnnotation(annotation: ScAnnotation): String = {
    val buffer = new StringBuilder("@")

    val constrInvocation = annotation.constructorInvocation
    val typ              = constrInvocation.typeElement.`type`().getOrAny
    buffer.append(defaultPresentationStringForScalaType(typ))

    val arguments = annotation.annotationExpr.getAnnotationParameters
    if (arguments.nonEmpty)
      buffer.append(arguments.iterator.map(a => escape(a.getText)).mkString("(", ", ", ")"))

    buffer.result
  }

  private def parameterAnnotations(param: ScParameter): String = {
    val annotationsRendered = param.annotations.iterator.map(renderAnnotation)
    val separator           = " "
    val suffix              = if (annotationsRendered.nonEmpty) separator else ""
    annotationsRendered.mkString("", separator, suffix)
  }

  private def parameterTypeAnnotations(param: ScParameter): String = {
    val typeText          = defaultPresentationStringForScalaType(param.`type`().getOrAny)
    val typeTextDecorated = decoratedParameterType(param, typeText)
    s": $typeTextDecorated"
  }

  private def decoratedParameterType(param: ScParameter, typeText: String): String = {
    val buffer = new StringBuilder

    if (param.isCallByNameParameter) {
      buffer.append(ScalaPsiUtil.functionArrow(param.getProject))
      buffer.append(" ")
    }

    buffer.append(typeText)

    if (param.isRepeatedParameter)
      buffer.append("*")

    if (param.isDefaultParam) {
      buffer.append(" = ")
      param.getDefaultExpressionInSource match {
        case Some(expr) =>
          val text: String = expr.getText.replace(" /* compiled code */ ", "")
          val cutTo        = 20
          buffer.append(text.substring(0, text.length.min(cutTo)))
          if (text.length > cutTo)
            buffer.append("...")
        case _ =>
          buffer.append("...")
      }
    }
    buffer.toString()
  }

  private def escape(text: String): String = StringEscapeUtils.escapeHtml(text)
}
