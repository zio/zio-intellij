package zio.intellij

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter

package object inspections {

  object collectionMethods {
    private[inspections] val `.map` = invocation("map").from(likeCollectionClasses)
  }

  object zioMethods {
    private[inspections] val `.*>`         = invocation("*>").from(zioClasses)
    private[inspections] val `.as`         = invocation("as").from(zioClasses)
    private[inspections] val `.map`        = invocation("map").from(zioClasses)
    private[inspections] val `.flatMap`    = invocation("flatMap").from(zioClasses)
    private[inspections] val `.mapError`   = invocation("mapError").from(zioClasses)
    private[inspections] val `.orElseFail` = invocation("orElseFail").from(zioClasses)
    private[inspections] val `.catchAll`   = invocation("catchAll").from(zioClasses)
    private[inspections] val `.foldCause`  = invocation("foldCause").from(zioClasses)
    private[inspections] val `.foldCauseM` = invocation("foldCauseM").from(zioClasses)

    private[inspections] val `assert` = unqualified("assert").from(zioClasses)
  }

  val zioClasses: Array[String] = Array("zio.ZIO", "zio.test._")

  def invocation(methodName: String)  = new Qualified(methodName == _)
  def unqualified(methodName: String) = new Unqualified(methodName == _)

  def fromZio(r: ScExpression): Boolean =
    isOfClassFrom(r, zioClasses)

  class ZIOMemberReference(refName: String) {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case `zioRef`(ref, e) if ref.refName == refName => Some(e)
      case _                                          => None
    }
  }

  val `ZIO.unit`          = new ZIOMemberReference("unit")
  val `ZIO.succeed`       = new ZIOMemberReference("succeed")
  val `ZIO.fail`          = new ZIOMemberReference("fail")
  val `ZIO.collectAll`    = new ZIOMemberReference("collectAll")
  val `ZIO.collectAllPar` = new ZIOMemberReference("collectAllPar")

  object unit {

    def unapply(expr: ScExpression): Boolean = expr match {
      case _: ScUnitExpr => true
      case _             => false
    }
  }

  object zioRef {

    def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] = expr match {
      case ref @ ScReferenceExpression(_) =>
        ref.resolve() match {
          case _: ScReferencePattern if fromZio(expr) => Some((ref, expr))
          case _                                      => None
        }
      case MethodRepr(_, _, Some(ref), Seq(e)) =>
        ref.resolve() match {
          case _ if fromZio(expr) => Some((ref, e))
          case _                  => None
        }
      // multiple argument lists
      case ScMethodCall(ScMethodCall(ref @ ScReferenceExpression(_), Seq(_)), Seq(_)) if fromZio(expr) =>
        Some((ref, expr))
      case _ => None
    }
  }

  object lambda {

    def unapply(expr: ScExpression): Option[(Seq[ScParameter], Option[ScExpression])] = expr match {
      case ScFunctionExpr(params @ Seq(_), res @ Some(_)) =>
        Some(params, res.map(stripped))
      case _ => None
    }
  }

  object `_ => x` {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case lambda(Seq(x), res) if underscore(x) => res
      case _                                    => None
    }

    // todo there must be a better way!
    def underscore(x: ScParameter): Boolean =
      x.isWildcard
  }

  // todo deal with this nasty duplication.
  // I want to be able somehow select the matched extractor dynamically
  object `_ => ()` {

    def unapply(expr: ScExpression): Boolean = expr match {
      case lambda(_, Some(unit())) => true
      case _                       => false
    }
  }

  object `_ => ZIO.unit` {

    def unapply(expr: ScExpression): Boolean = expr match {
      case lambda(_, Some(`ZIO.unit`(_))) => true
      case _                              => false
    }
  }

  object IsDeprecated {

    def unapply(expr: ScExpression): Option[PsiAnnotation] = expr match {
      case ScMethodCall(ref: ScReferenceExpression, _) =>
        ref.resolve() match {
          case fn: ScFunctionDefinition if fn.isDeprecated => Some(fn.findAnnotation("scala.deprecated"))
          case _                                           => None
        }
      case _ => None
    }
  }
}
