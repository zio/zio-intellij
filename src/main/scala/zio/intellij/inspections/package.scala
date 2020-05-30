package zio.intellij

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.codeInspection.collections.{isOfClassFrom, _}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.ScType

package object inspections {

  object collectionMethods {
    private[inspections] val `.map` = invocation("map").from(likeCollectionClasses)
  }

  object zioMethods {
    private[inspections] val `.*>`           = invocation("*>").from(zioLikePackages)
    private[inspections] val `.as`           = invocation("as").from(zioLikePackages)
    private[inspections] val `.map`          = invocation("map").from(zioLikePackages)
    private[inspections] val `.flatMap`      = invocation("flatMap").from(zioLikePackages)
    private[inspections] val `.flatMapError` = invocation("flatMapError").from(zioLikePackages)
    private[inspections] val `.mapError`     = invocation("mapError").from(zioLikePackages)
    private[inspections] val `.orElseFail`   = invocation("orElseFail").from(zioLikePackages)
    private[inspections] val `.catchAll`     = invocation("catchAll").from(zioLikePackages)
    private[inspections] val `.fold`         = invocation("fold").from(zioLikePackages)
    private[inspections] val `.foldCause`    = invocation("foldCause").from(zioLikePackages)
    private[inspections] val `.foldCauseM`   = invocation("foldCauseM").from(zioLikePackages)
    private[inspections] val `.tap`          = invocation("tap").from(zioLikePackages)
    private[inspections] val `.tapError`     = invocation("tapError").from(zioLikePackages)

    private[inspections] val `assert` = unqualified("assert").from(zioLikePackages)
  }

  object hasMethods {
    val zioHasLikeClasses: Array[String] = Array("zio.Has", "zio.Has._")

    private[inspections] val `.get` = invocation("get").from(zioHasLikeClasses)
  }

  val zioLikePackages: Array[String] = Array("zio._")

  def invocation(methodName: String)  = new Qualified(methodName == _)
  def unqualified(methodName: String) = new Unqualified(methodName == _)

  def fromZio(r: ScExpression): Boolean =
    isOfClassFrom(r, zioLikePackages)

  def fromZio(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioLikePackages)

  class ZIOStaticMemberReference(refName: String) {

    private def matchesRefName(ref: ScReferenceExpression) =
      if (ref.refName == refName) true
      else
        ref.resolve() match {
          // handles the 'apply' case when called with ZIO(x)
          case n: ScNamedElement if n.name == refName => true
          case _                                      => false
        }

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case ref @ ScReferenceExpression(_) if matchesRefName(ref) =>
        ref.smartQualifier match {
          case Some(ZIOStaticMemberReference()) => Some(expr)
          case _                                => None
        }
      case MethodRepr(_, _, Some(ref), Seq(e)) if matchesRefName(ref) =>
        ref match {
          case ZIOStaticMemberReference() => Some(e)
          case _                          => None
        }
      case _ => None
    }
  }

  object ZIOStaticMemberReference {
    // todo make me not do this
    val zioTypes = Set("zio.ZIO", "zio.UIO", "zio.RIO", "zio.URIO", "zio.IO", "zio.Task")

    def unapply(ref: ScReferenceExpression): Boolean =
      ref.resolve() match {
        case t: ScTemplateDefinition if zioTypes.contains(t.qualifiedName)                 => true
        case f: ScFunctionDefinition if zioTypes.contains(f.containingClass.qualifiedName) => true
        case _                                                                             => false
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

  val `ZIO.apply`         = new ZIOStaticMemberReference("apply")
  val `ZIO.unit`          = new ZIOStaticMemberReference("unit")
  val `ZIO.succeed`       = new ZIOStaticMemberReference("succeed")
  val `ZIO.fail`          = new ZIOStaticMemberReference("fail")
  val `ZIO.collectAll`    = new ZIOStaticMemberReference("collectAll")
  val `ZIO.collectAllPar` = new ZIOStaticMemberReference("collectAllPar")
  val `ZIO.sleep`         = new ZIOStaticMemberReference("sleep")
  val `ZIO.effect`        = new ZIOStaticMemberReference("effect")
  val `ZIO.effectTotal`   = new ZIOStaticMemberReference("effectTotal")
  val `ZIO.access`        = new ZIOStaticMemberReference("access")

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
          case _: ScReferencePattern | _: ScFunctionDefinition if fromZio(expr) => Some((ref, expr))
          case _                                                                => None
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

  val exitCodeSuccess = new ExitCode("success")
  val exitCodeFailure = new ExitCode("failure")

  class ExitCode(refName: String) {

    def unapply(expr: ScExpression): Boolean = expr match {
      case ref @ ScReferenceExpression(_) if ref.refName == refName =>
        ref.resolve() match {
          case p: ScReferencePattern if p.containingClass.qualifiedName == "zio.ExitCode" => true
          case _                                                                          => false
        }
      case _ => false
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
