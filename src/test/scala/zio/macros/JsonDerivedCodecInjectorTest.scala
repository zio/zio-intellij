package zio.macros

import org.junit.Assert.assertEquals

class JsonDerivedCodecForCaseClassInjectorTest extends MacrosTest {

  override protected def code: String =
    s"""import zio.json._
       |
       |@jsonDerive
       |final case class Foo(i: Int)
       |
       |object F${CARET}oo""".stripMargin

  def test_generates_codec_for_case_class(): Unit =
    assertEquals(
      "implicit def codecForFoo: _root_.zio.json.JsonCodec[Foo] = _root_.scala.Predef.???",
      method("codecForFoo").getText
    )

}

class JsonDerivedCodecForTypedCaseClassInjectorTest extends MacrosTest {

  override protected def code: String =
    s"""import zio.json._
       |
       |@jsonDerive
       |final case class Foo[K, V](k: K, v: V)
       |
       |object F${CARET}oo""".stripMargin

  def test_generates_codec_for_typed_case_class(): Unit =
    assertEquals(
      "implicit def codecForFoo[K: _root_.zio.json.JsonCodec,V: _root_.zio.json.JsonCodec]:" +
        " _root_.zio.json.JsonCodec[Foo[K,V]] = _root_.scala.Predef.???",
      method("codecForFoo").getText
    )

}

class JsonDerivedCodecForSealedTraitInjectorTest extends MacrosTest {

  override protected def code: String =
    s"""import zio.json._
       |
       |@jsonDerive
       |sealed trait Foo
       |
       |object F${CARET}oo""".stripMargin

  def test_generates_codec_for_sealed_trait(): Unit =
    assertEquals(
      "implicit def codecForFoo: _root_.zio.json.JsonCodec[Foo] = _root_.scala.Predef.???",
      method("codecForFoo").getText
    )

}
