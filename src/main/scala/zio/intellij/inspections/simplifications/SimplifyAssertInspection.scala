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
          case replace => replacement(expr, replace.mkString)
        }
      case _ => None
    }

  private def mkAssertion(body: ScExpression, assertion: ScExpression): Option[List[String]] = {
    def go(asrt: ScExpression, terms: List[String]): List[String] =
      asrt match {
        case equalTo(ScBooleanLiteral(bool))        => if (bool) body.getText :: Nil else Nil // TODO equalTo(false)
        case equalTo(expr)                          => go(expr, "==" :: terms) :+ body.getText
        case isGreaterThan(expr)                    => go(expr, ">" :: terms) :+ body.getText
        case isGreaterThanEqualTo(expr)             => go(expr, ">=" :: terms) :+ body.getText
        case isLessThan(expr)                       => go(expr, "<" :: terms) :+ body.getText
        case isLessThanEqualTo(expr)                => go(expr, "<=" :: terms) :+ body.getText
        case isSome()                               => Nil
        case isSome(expr)                           => go(expr, ".get" :: terms)
        case isEmpty()                              => s"${body.getText}.isEmpty" :: Nil
        case isNull()                               => s"${body.getText} == null" :: Nil
        case isTrue()                               => body.getText :: Nil
        case isFalse()                              => s"!(${body.getText})" :: Nil
        case contains(_) if !isIterable(body)       => Nil
        case contains(expr)                         => s"${body.getText}.contains(${go(expr, terms).mkString})" :: Nil
        case containsString(_) if !isString(body)   => Nil
        case containsString(expr)                   => s"${body.getText}.contains(${go(expr, terms).mkString})" :: Nil
        case exists(_) if !isIterable(body)         => Nil
        case exists(anything())                     => s"${body.getText}.nonEmpty" :: Nil
        case exists(equalTo(arg))                   => s"${body.getText}.contains(${arg.getText})" :: Nil
        case exists(expr)                           => s"${body.getText}.exists(_ ${go(expr, terms).init.reverse.mkString})" :: Nil
        case startsWith(_) if !isSeq(body)          => Nil
        case startsWith(expr)                       => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case startsWithString(_) if !isString(body) => Nil
        case startsWithString(expr)                 => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case endsWith(_) if !isSeq(body)            => Nil
        case endsWith(expr)                         => s"${body.getText}.endsWith(${go(expr, terms).mkString})" :: Nil
        case endsWithString(_) if !isString(body)   => Nil
        case endsWithString(expr)                   => s"${body.getText}.startsWith(${go(expr, terms).mkString})" :: Nil
        case other                                  => other.getText :: terms
      }

    val expr = go(assertion, Nil)
    Option.when(expr.nonEmpty)(expr.reverse)
  }
}
