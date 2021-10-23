package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScBooleanLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections.ZInspection
import zio.intellij.inspections.assertMethods._
import zio.intellij.utils._

final class SimplifyAssertInspection extends ZInspection(SimplifyEqualToType)

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
        case isEmptyString() if isString(body)            => s"${body.getText}.isEmpty" :: Nil
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
        case containsString(expr) if isString(body)       => s"${body.getText}.contains(${go(expr, terms).mkString})" :: Nil
        case exists(_) if !isIterable(body)               => Nil
        case exists(anything())                           => s"${body.getText}.nonEmpty" :: Nil
        case exists(equalTo(arg))                         => s"${body.getText}.contains(${arg.getText})" :: Nil
        case exists(expr)                                 => s"${body.getText}.exists(_ ${go(expr, terms).init.reverse.mkString})" :: Nil
        case startsWith(expr) if isSeq(body)              => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case startsWithString(expr) if isString(body)     => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case endsWith(expr) if isSeq(body)                => s"${body.getText}.endsWith(${go(expr, terms).mkString})" :: Nil
        case endsWithString(expr) if isString(body)       => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case other if !isAssertion(other)                 => other.getText :: terms
        case _                                            => Nil
      }

    val expr = go(assertion, Nil)
    Option.when(expr.nonEmpty)(expr.reverse)
  }

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
