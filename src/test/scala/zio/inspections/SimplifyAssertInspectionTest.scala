package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyAssertInspection

class SimplifyAssertInspectionTest extends ZSimplifyInspectionTest[SimplifyAssertInspection] {
  override protected val hint: String = "Replace with assertTrue"

  private def test(test: String, expected: String): Unit = {
    z(s"$START$test$END").assertHighlighted()
    val t = z(test)
    val e = z(expected)
    testQuickFix(t, e, hint)
  }

  def testEqualTo_true() =
    test("assert(1 == 1)(equalTo(true))", "assertTrue(1 == 1)")

  def testEqualTo_false() =
    z(s"${START}assert(1 == 1)(equalTo(false))$END").assertNotHighlighted()

  def testEqualTo_value() =
    test("""assert(a)(equalTo("hello"))""", """assertTrue(a == "hello")""")

  def test_isGreaterThan() =
    test("assert(2)(isGreaterThan(1))", "assertTrue(2 > 1)")

  def test_isGreaterThanEqualTo() =
    test("assert(2)(isGreaterThanEqualTo(1))", "assertTrue(2 >= 1)")

  def test_isLessThan() =
    test("assert(1)(isLessThan(2))", "assertTrue(1 < 2)")

  def test_isLessThanEqualTo() =
    test("assert(1)(isLessThanEqualTo(2))", "assertTrue(1 <= 2)")

  def test_isSome() =
    test("assert(o)(isSome(equalTo(1)))", "assertTrue(o.get == 1)")

  def test_isEmpty() =
    test("assert(List(1))(isEmpty)", "assertTrue(List(1).isEmpty)")

  def test_isNull() =
    test("assert(a)(isNull)", "assertTrue(a == null)")

  def test_isTrue() =
    test("assert(List(1,2,3).isEmpty)(isTrue)", "assertTrue(List(1,2,3).isEmpty)")

  def test_isFalse() =
    test("assert(List(1,2,3).isEmpty)(isFalse)", "assertTrue(!(List(1,2,3).isEmpty))")

  def test_contains_non_iterable() =
    z(s"""${START}assert("abc")(contains("a"))$END""").assertNotHighlighted()

  def test_contains_iterable() =
    test("assert(List(1,2,3))(contains(2))", "assertTrue(List(1,2,3).contains(2))")

  def test_containsString_non_string() =
    z(s"""${START}assert(List("a"))(containsString("a"))$END""").assertNotHighlighted()

  def test_containsString() =
    test("""assert("abc")(containsString("ab"))""", """assertTrue("abc".contains("ab"))""")

  def test_exists_non_iterable() =
    z(s"""${START}assert("abc")(exists(equalTo("a")))$END""").assertNotHighlighted()

  def test_exists_anything() =
    test("assert(List(1,2,3))(exists(anything))", "assertTrue(List(1,2,3).nonEmpty)")

  def test_exists_equalTo() =
    test("assert(List(1,2,3))(exists(equalTo(2)))", "assertTrue(List(1,2,3).contains(2))")

  def test_exists_expr() =
    test("assert(List(1,2,3))(exists(isGreaterThanEqualTo(2)))", "assertTrue(List(1,2,3).exists(_ >= 2))")

  def test_startsWith_non_seq() =
    z(s"""${START}assert("abc")(startsWith("ab"))$END""").assertNotHighlighted()

  def test_startsWith() =
    test("assert(List(1,2,3))(startsWith(List(1,2)))", "assertTrue(List(1,2,3).startsWith(List(1,2)))")

  def test_startsWithString_non_string() =
    z(s"""${START}assert(List("abc"))(startsWithString(List("ab")))$END""").assertNotHighlighted()

  def test_startsWithString() =
    test("""assert(a)(startsWithString("ab"))""", """assertTrue(a.startsWith("ab"))""")

  def test_startsWithString_literal() =
    test("""assert("abc")(startsWithString("ab"))""", """assertTrue("abc".startsWith("ab"))""")

  def test_not() =
    test("""assert("a")(not(equalTo("b"))""", """assertTrue(!("a" == "b"))""")

  def test_isEmptyString_non_str() =
    z(s"""${START}assert(List())(isEmptyString)$END""").assertNotHighlighted()

  def test_isEmptyString() =
    test("""assert(a)(isEmptyString)""", """assertTrue(a.isEmpty)""")

  def test_isNegative() =
    test("""assert(1)(isNegative)""", """assertTrue(1 < 0)""")

  def test_isPositive() =
    test("""assert(1)(isPositive)""", """assertTrue(1 > 0)""")

  def test_isNonEmpty() =
    test("""assert(List(1,3))(isNonEmpty)""", """assertTrue(List(1,3).nonEmpty)""")

  def test_isNaNDouble() =
    test("""assert(1.0d)(isNaNDouble)""", """assertTrue(1.0d.isNaN)""")

  def test_isPosInfinityDouble() =
    test("""assert(1.0d)(isPosInfinityDouble)""", """assertTrue(1.0d.isPosInfinity)""")

  def test_isNegInfinityDouble() =
    test("""assert(1.0d)(isNegInfinityDouble)""", """assertTrue(1.0d.isNegInfinity)""")

  def test_isFiniteDouble() =
    test("""assert(1.0d)(isFiniteDouble)""", """assertTrue(1.0d <= Double.MaxValue)""")

  def test_isInfiniteDouble() =
    test("""assert(1.0d)(isInfiniteDouble)""", """assertTrue(1.0d.isInfinite)""")

  def test_isPosInfinityFloat() =
    test("""assert(1.0)(isPosInfinityFloat)""", """assertTrue(1.0.isPosInfinity)""")

  def test_isNegInfinityFloat() =
    test("""assert(1.0)(isNegInfinityFloat)""", """assertTrue(1.0.isNegInfinity)""")

  def test_isFiniteFloat() =
    test("""assert(1.0)(isFiniteFloat)""", """assertTrue(1.0 <= Float.MaxValue)""")

  def test_isInfiniteFloat() =
    test("""assert(1.0)(isInfiniteFloat)""", """assertTrue(1.0.isInfinite)""")
}
