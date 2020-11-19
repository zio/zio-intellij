package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScParameterizedTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTrait
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}
import zio.intellij.utils.TypeCheckUtils.{fromManaged, fromZio}

abstract class ModulePatternAccessibleMBase extends ModulePatternAccessibleBase {

  protected val expectedTypeParams: Long

  override protected def members(annotation: ScAnnotation, serviceTrait: ScTrait): Members = new Members(serviceTrait) {

    private val typeToReplaceWith: Option[ScType] = annotation.typeElement match {
      case ScParameterizedTypeElement(_, Seq(Typeable(tpe))) if fromZio(tpe) || fromManaged(tpe) => Some(tpe)
      case _                                                                                     => None
    }

    private val typeToReplaceWithStr: Option[String] =
      typeToReplaceWith.map(_.canonicalText)

    private val typeParamToReplace: Option[ScTypeParam] =
      serviceTrait.typeParameters.find(_.typeParameters.size == expectedTypeParams)

    override protected def modifyType(typeRes: ScType): ScType = typeRes match {
      case p: ScParameterizedType if typeParamToReplace.exists(_.name == p.designator.canonicalText) =>
        typeToReplaceWith.map(ScParameterizedType(_, p.typeArguments)).getOrElse(p)
      case other => other
    }

    override protected val typeArgsForService: Seq[String] =
      serviceTrait.typeParameters.map {
        case tpeToReplace if typeParamToReplace.contains(tpeToReplace) =>
          typeToReplaceWithStr.getOrElse(tpeToReplace.name)
        case other => other.name
      }

    override protected val typeArgsForAccessors: Seq[ScTypeParam] =
      serviceTrait.typeParameters.filterNot(typeParamToReplace.contains(_))
  }

}
