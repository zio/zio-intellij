package zio.intellij.utils

import com.sun.jdi._
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

package object jdi {

  def getFieldValue(obj: ObjectReference, fieldName: String): Value =
    obj.getValue(obj.referenceType().fieldByName(fieldName))

  def convertBooleanValue(value: Value): Option[Boolean] =
    value match {
      case b: BooleanValue => Some(b.value())
      case _               => None
    }

  def convertIntegerValue(value: Value): Option[Int] =
    value match {
      case l: IntegerValue => Some(l.value())
      case _               => None
    }

  def convertLongValue(value: Value): Option[Long] =
    value match {
      case l: LongValue => Some(l.value())
      case _            => None
    }

  def convertStringValue(value: Value): Option[String] =
    value match {
      case string: StringReference =>
        Some(string.value())
      case _ => None
    }

  def convertOption[T](optionValue: Value)(inner: Value => Option[T]): Option[T] =
    optionValue match {
      case o: ObjectReference if o.`type`().name() == OptionRef.SomeName =>
        val value = getFieldValue(o, OptionRef.ValueField)
        inner(value)
      case _ => None
    }

  def convertScalaSeq(value: Value)(implicit languageLevel: ScalaLanguageLevel): List[Value] = {
    @tailrec
    def convertList(value: Value, acc: List[Value]): List[Value] =
      value match {
        case obj: ObjectReference if obj.`type`().name() == ListRef.ConsName =>
          val head = getFieldValue(obj, ListRef.HeadField)
          val tail = getFieldValue(obj, ListRef.TailField)
          convertList(tail, head :: acc)
        case _ => acc.reverse
      }

    value match {
      case obj: ObjectReference if obj.`type`().name() == ListRef.ConsName => convertList(value, Nil)
      case array: ArrayReference                                           => array.getValues.asScala.toList
      case _                                                               => Nil
    }
  }

  private[jdi] object OptionRef {
    // classes
    val SomeName: String = "scala.Some"

    // fields
    val ValueField: String = "value"
  }

  private[jdi] object ListRef {
    // classes
    val ConsName = "scala.collection.immutable.$colon$colon"

    // fields
    val HeadField = "head"

    def TailField(implicit languageLevel: ScalaLanguageLevel): String =
      if (languageLevel < ScalaLanguageLevel.Scala_2_13) "tl" else "next"
  }

}
