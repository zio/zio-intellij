package zio.intellij.inspections.simplifications

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.{ScBooleanLiteral, ScStringLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.api.UndefinedType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{api, ScParameterizedType, ScType}
import org.jetbrains.plugins.scala.project.ProjectContext
import zio.intellij.inspections.ZInspection
import zio.intellij.inspections.assertMethods._
import zio.intellij.utils.StringUtils.ScExpressionExt
import zio.intellij.utils._

final class SimplifyAssertInspection extends ZInspection(SimplifyEqualToType, SimplifyAssertTrueChain)

object SimplifyEqualToType extends SimplificationType {
  override def hint: String = "Replace with assertTrue"

  def replacement(expr: ScExpression, body: String): Simplification =
    replace(expr).withText(s"assertTrue($body").highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case assert(_, body, assertion) =>
        mkAssertion(body, assertion).collect {
          case replace => replacement(expr, replace.mkString)
        }
      case _ => None
    }

  private def mkAssertion(body: ScExpression, assertion: ScExpression): Option[List[String]] = {
    def go(asrt: ScExpression, terms: List[String]): List[String] =
      // scalafmt: { maxColumn = 200 }
      asrt match {
        case Unsupported()                                => Nil
        case equalTo(ScBooleanLiteral(bool))              => if (bool) body.getText :: Nil else Nil // TODO equalTo(false)
        case equalTo(expr)                                => go(expr, "==" :: terms) :+ body.getText
        case isGreaterThan(expr)                          => go(expr, ">" :: terms) :+ body.getText
        case isGreaterThanEqualTo(expr)                   => go(expr, ">=" :: terms) :+ body.getText
        case isLessThan(expr)                             => go(expr, "<" :: terms) :+ body.getText
        case isLessThanEqualTo(expr)                      => go(expr, "<=" :: terms) :+ body.getText
        case isSome(expr)                                 => go(expr, ".get" :: terms)
        case not(expr)                                    => s"!(${go(expr, terms).reverse.mkString})" :: Nil
        case isEmpty()                                    => s"${body.getText}.isEmpty" :: Nil
        case isEmptyString() if isStringHax(body)         => s"${body.getText}.isEmpty" :: Nil
        case isNonEmpty()                                 => s"${body.getText}.nonEmpty" :: Nil
        case isNegative()                                 => s"${body.getText} < 0" :: Nil
        case isPositive()                                 => s"${body.getText} > 0" :: Nil
        case isNaNDouble() | isNaNFloat()                 => s"${body.getText}.isNaN" :: Nil
        case isPosInfinityDouble() | isPosInfinityFloat() => s"${body.getText}.isPosInfinity" :: Nil
        case isNegInfinityDouble() | isNegInfinityFloat() => s"${body.getText}.isNegInfinity" :: Nil
        case isFiniteDouble()                             => s"${body.getText} <= Double.MaxValue" :: Nil
        case isFiniteFloat()                              => s"${body.getText} <= Float.MaxValue" :: Nil
        case isInfiniteDouble() | isInfiniteFloat()       => s"${body.getText}.isInfinite" :: Nil
        case isNull()                                     => s"${body.getText} == null" :: Nil
        case isTrue()                                     => body.getText :: Nil
        case isFalse()                                    => s"!(${body.getText})" :: Nil
        case contains(expr) if isIterable(body)           => s"${body.getText}.contains(${go(expr, terms).mkString})" :: Nil
        case containsString(expr) if isStringHax(body)    => s"${body.getText}.contains(${go(expr, terms).mkString})" :: Nil
        case exists(_) if !isIterable(body)               => Nil
        case exists(anything())                           => s"${body.getText}.nonEmpty" :: Nil
        case exists(equalTo(arg))                         => s"${body.getText}.contains(${arg.getText})" :: Nil
        case exists(expr)                                 => s"${body.getText}.exists(_ ${go(expr, terms).init.reverse.mkString})" :: Nil
        case startsWith(expr) if isSeq(body)              => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case startsWithString(expr) if isStringHax(body)  => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case endsWith(expr) if isSeq(body)                => s"${body.getText}.endsWith(${go(expr, terms).mkString})" :: Nil
        case endsWithString(expr) if isStringHax(body)    => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case other if !isAssertion(other)                 => other.getText :: terms
        case _                                            => Nil
      }

    val expr = go(assertion, Nil)
    Option.when(expr.nonEmpty)(expr.reverse)
  }

  // TODO this is a hack until I figure out why `isString` doesn't work in tests
  private[this] def isStringHax: ScExpression => Boolean =
    isExpressionOfType("java.lang.String", "_root_.scala.Predef.String")

  private def isExpressionOfType(fqns: String*): ScExpression => Boolean = {
    case ScStringLiteral(_) => true
    case expression @ Typeable(scType) =>
      fqns.exists(conforms(scType, _)(expression)) ||
        fqns.contains(scType.canonicalText)
    case _ => false
  }

  private def conforms(scType: ScType, fqn: String)(implicit projectContext: ProjectContext): Boolean =
    (scType != api.Null) && (scType != api.Nothing) && {
      ElementScope(projectContext)
        .getCachedClass(fqn)
        .map(c => createParameterizedType(c))
        .exists(v => scType.conforms(v))
    }

  private def createParameterizedType(clazz: PsiClass) = {
    val designatorType = ScDesignatorType(clazz)
    clazz.getTypeParameters match {
      case Array()    => designatorType
      case parameters => ScParameterizedType(designatorType, parameters.map(UndefinedType(_)).toIndexedSeq)
    }
  }
  // end hack

  object Unsupported {
    def unapply(expr: ScExpression): Boolean =
      expr match {
        case isSome()               => true
        case isLeft() | isLeft(_)   => true
        case isRight() | isRight(_) => true
        case _                      => false
      }
  }
}

object SimplifyAssertTrueChain extends SimplificationType {

  override def hint: String = "Replace with assertTrue(conditions: _*)"

  def replacement(expr: ScExpression, body: FlattenedAssertions): Simplification = {
    val assertTrue = s"assertTrue(${body.assertions.map(_.getBracedText).mkString(", ")})"
    // we can safely combine via `&&` here since `||` is parsed differently and won't get here
    val assert = body.remainder.fold(assertTrue)(r => s"${r.getText} && $assertTrue")
    replace(expr).withText(assert).highlightFrom(body.remainder.getOrElse(expr))
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    (expr, expr.getParent) match {
      // if there's an `assertTrue` parent, we've already handled this case
      case (_, stripped(parent @ (_ && assertTrue(_ @_*)))) if parent ne expr => None
      case (_ `&&` assertTrue(topLevel @ _*), _) =>
        val assertions = extractAssertions(expr)
        Option.when(assertions.assertions != topLevel)(replacement(expr, assertions))
      case _ => None
    }

  @scala.annotation.tailrec
  private def extractAssertions(expr: ScExpression, acc: Seq[ScExpression] = Vector.empty): FlattenedAssertions =
    expr match {
      case left `&&` assertTrue(right @ _*) => extractAssertions(left, right ++ acc)
      case assertTrue(exprs @ _*)           => FlattenedAssertions(exprs ++ acc, None)
      case assert                           => FlattenedAssertions(acc, Some(assert))
    }

  private final case class FlattenedAssertions(assertions: Seq[ScExpression], remainder: Option[ScExpression])

}
