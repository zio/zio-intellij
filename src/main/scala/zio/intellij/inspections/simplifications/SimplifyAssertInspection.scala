package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScBooleanLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.scalajs.transformation.ir.BooleanLiteral
import zio.intellij.inspections.ZInspection
import zio.intellij.inspections.assertMethods._
import zio.intellij.utils.isIterable

final class SimplifyAssertInspection extends ZInspection(SimplifyEqualToType)

object SimplifyEqualToType extends SimplificationType {
  override def hint: String = "Replace with assertTrue"

  def replacement(expr: ScExpression, body: String): Simplification =
    replace(expr).withText(s"assertTrue($body").highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case assert(_, body, assertion) =>
        mkAssertion(body, assertion).collect {
          case terms =>
            val replace = body.getText :: terms
            replacement(expr, replace.mkString)
        }
      case _ => None
    }

  private def mkAssertion(body: ScExpression, assertion: ScExpression): Option[List[String]] = {
    def go(asrt: ScExpression, terms: List[String]): List[String] =
      asrt match {
        case equalTo(ScBooleanLiteral(bool))        => if (bool) "" :: terms else terms // TODO equalTo(false)
        case equalTo(expr)                          => go(expr, "==" :: terms)
        case isGreaterThan(expr)                    => go(expr, ">" :: terms)
        case isGreaterThanEqualTo(expr)             => go(expr, ">=" :: terms)
        case isLessThan(expr)                       => go(expr, "<" :: terms)
        case isLessThanEqualTo(expr)                => go(expr, "<=" :: terms)
        case isSome(expr)                           => go(expr, ".get" :: terms)
        case isEmpty()                              => ".isEmpty" :: terms
        case isNull()                               => "== null" :: terms
        case contains(_) if !isIterable(body)       => Nil
        case contains(expr)                         => s".contains(${go(expr, terms).mkString})" :: terms
        case containsString(_) if !isString(body)   => Nil
        case containsString(expr)                   => s".contains(${go(expr, terms).mkString})" :: terms
        case exists(_) if !isIterable(body)         => Nil
        case exists(anything())                     => ".nonEmpty" :: terms
        case exists(equalTo(arg))                   => s".contains(${arg.getText})" :: terms
        case exists(expr)                           => s".exists(_ ${go(expr, terms).reverse.mkString})" :: terms
        case startsWith(_) if !isSeq(body)          => Nil
        case startsWith(expr)                       => s".startsWith(${go(expr, terms).mkString})" :: terms
        case startsWithString(_) if !isString(body) => Nil
        case startsWithString(expr)                 => s".startsWith(${go(expr, terms).mkString})" :: terms
        case endsWith(_) if !isSeq(body)            => Nil
        case endsWith(expr)                         => s".endsWith(${go(expr, terms).mkString})" :: terms
        case endsWithString(_) if !isString(body)   => Nil
        case endsWithString(expr)                   => s".startsWith(${go(expr, terms).mkString})" :: terms
        case other                                  => other.getText :: terms
      }

    val expr = go(assertion, Nil).reverse
    Option.when(expr.nonEmpty)(expr)
  }
}
