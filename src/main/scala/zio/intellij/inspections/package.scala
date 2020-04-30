package zio.intellij

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.codeInspection.collections.{isOfClassFrom, _}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember

package object inspections {

  object collectionMethods {
    private[inspections] val `.map` = invocation("map").from(likeCollectionClasses)
  }

  object zioMethods {
    private[inspections] val `.*>`           = invocation("*>").from(zioClasses)
    private[inspections] val `.as`           = invocation("as").from(zioClasses)
    private[inspections] val `.map`          = invocation("map").from(zioClasses)
    private[inspections] val `.flatMap`      = invocation("flatMap").from(zioClasses)
    private[inspections] val `.flatMapError` = invocation("flatMapError").from(zioClasses)
    private[inspections] val `.mapError`     = invocation("mapError").from(zioClasses)
    private[inspections] val `.orElseFail`   = invocation("orElseFail").from(zioClasses)
    private[inspections] val `.catchAll`     = invocation("catchAll").from(zioClasses)
    private[inspections] val `.foldCause`    = invocation("foldCause").from(zioClasses)
    private[inspections] val `.foldCauseM`   = invocation("foldCauseM").from(zioClasses)
    private[inspections] val `.tap`          = invocation("tap").from(zioClasses)
    private[inspections] val `.tapError`     = invocation("tapError").from(zioClasses)

    private[inspections] val `assert` = unqualified("assert").from(zioTestClasses)
  }

  val zioClasses: Array[String]     = Array("zio._")
  val zioTestClasses: Array[String] = Array("zio.test._")

  def invocation(methodName: String)  = new Qualified(methodName == _)
  def unqualified(methodName: String) = new Unqualified(methodName == _)

  def fromZio(r: ScExpression): Boolean =
    isOfClassFrom(r, zioClasses)

  def fromZioTest(r: ScExpression): Boolean =
    isOfClassFrom(r, zioTestClasses)

  class ZIOMemberReference(refName: String) {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case `zioRef`(ref, e) =>
        if (ref.refName == refName) Some(e)
        else
          ref.resolve() match {
            case n: ScNamedElement if n.name == refName => Some(e) // handles the 'apply' case when called with ZIO(x)
            case _                                      => None
          }
      case _ => None
    }
  }

  class TypeReference(typeFQNs: Set[String]) {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case MethodRepr(_, _, Some(ref), Seq(_)) =>
        ref.resolve() match {
          case m: ScMember if typeFQNs.contains(m.containingClass.qualifiedName) => Some(expr)
          case _                                                                 => None
        }
      case MethodRepr(_, Some(ref @ ScReferenceExpression(_)), None, Seq(_)) if isOfClassFrom(ref, typeFQNs.toArray) =>
        Some(expr)
      case ref @ ScReferenceExpression(_) if isOfClassFrom(expr, typeFQNs.toArray) => Some(ref)
      case _                                                                       => None
    }
  }

  val scalaFuture = new TypeReference(Set("scala.concurrent.Future"))
  val scalaTry    = new TypeReference(Set("scala.util.Try", "scala.util.Success", "scala.util.Failure"))
  val scalaOption = new TypeReference(Set("scala.Option", "scala.Some", "scala.None"))
  val scalaEither = new TypeReference(Set("scala.util.Either", "scala.util.Left", "scala.util.Right"))

  val `ZIO.apply`         = new ZIOMemberReference("apply")
  val `ZIO.unit`          = new ZIOMemberReference("unit")
  val `ZIO.succeed`       = new ZIOMemberReference("succeed")
  val `ZIO.fail`          = new ZIOMemberReference("fail")
  val `ZIO.collectAll`    = new ZIOMemberReference("collectAll")
  val `ZIO.collectAllPar` = new ZIOMemberReference("collectAllPar")
  val `ZIO.sleep`         = new ZIOMemberReference("sleep")
  val `ZIO.effect`        = new ZIOMemberReference("effect")
  val `ZIO.effectTotal`   = new ZIOMemberReference("effectTotal")

  object unit {

    def unapply(expr: ScExpression): Boolean = expr match {
      case _: ScUnitExpr => true
      case _             => false
    }
  }

  sealed trait baseZioRef {

    def extract(expr: ScExpression, isZioRef: ScExpression => Boolean): Option[(ScReferenceExpression, ScExpression)] =
      expr match {
        case ref @ ScReferenceExpression(_) =>
          ref.resolve() match {
            case _: ScReferencePattern | _: ScFunctionDefinition if isZioRef(expr) => Some((ref, expr))
            case _                                                                 => None
          }
        case MethodRepr(_, _, Some(ref), Seq(e)) =>
          ref.resolve() match {
            case _ if isZioRef(expr) => Some((ref, e))
            case _                   => None
          }
        // multiple argument lists
        case ScMethodCall(ScMethodCall(ref @ ScReferenceExpression(_), Seq(_)), Seq(_)) if isZioRef(expr) =>
          Some((ref, expr))
        case _ => None
      }
  }

  object zioRef extends baseZioRef {
    def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] = extract(expr, fromZio)
  }

  object zioTestRef extends baseZioRef {
    def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] = extract(expr, fromZioTest)
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
