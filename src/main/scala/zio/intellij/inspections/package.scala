package zio.intellij

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter

package object inspections {
  val zioClasses: Array[String] = Array("zio.ZIO", "zio.test")

  def invocation(methodName: String) = new Qualified(methodName == _)
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
  val `ZIO.collectAll`    = new ZIOMemberReference("collectAll")
  val `ZIO.collectAllPar` = new ZIOMemberReference("collectAllPar")

  object `()` {
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
      case _ => None
    }
  }

  object `_ => x` {
    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case ScFunctionExpr(Seq(x), res @ Some(_)) if underscore(x) => res
      case _                                                      => None
    }

    // todo there must be a better way!
    def underscore(x: ScParameter): Boolean =
      x.isWildcard
  }

  // todo deal with this nasty duplication.
  // I want to be able somehow select the matched extractor dynamically
  object `_ => ()` {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ScFunctionExpr(_, Some(result)) =>
        stripped(result) match {
          case `()`() => true
          case _      => false
        }
      case _ => false
    }
  }

  object `_ => ZIO.unit` {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ScFunctionExpr(_, Some(result)) =>
        stripped(result) match {
          case `ZIO.unit`(_) => true
          case _             => false
        }
      case _ => false
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
