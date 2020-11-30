package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTrait
import org.jetbrains.plugins.scala.lang.psi.types.ScType

class ModulePatternAccessible extends ModulePatternAccessibleBase {
  protected val macroName: String = "zio.macros.accessible"

  override protected def members(annotation: ScAnnotation, serviceTrait: ScTrait): Members = new Members(serviceTrait) {
    protected def modifyType(typeRes: ScType): ScType = typeRes

    protected val typeArgsForService: Seq[String] = serviceTrait.typeParameters.map(_.name)

    protected val typeArgsForAccessors: Seq[ScTypeParam] = serviceTrait.typeParameters
  }

}
