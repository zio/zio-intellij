package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyUnlessInspection

class SimplifyUnlessInspectionTest extends ZSimplifyInspectionTest[SimplifyUnlessInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .unless"

  def test_unless_reference_to_val(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |val b = ZIO.succeed(42)
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (a) ZIO.unit else b$END")).assertHighlighted()
      val text   = z(base(s"if (a) ZIO.unit else b"))
      val result = z(base(s"b.unless(a)"))
      testQuickFixes(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) b else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (!a) b else ZIO.unit"))
      val result = z(base(s"b.unless(a)"))
      testQuickFixes(text, result, hint)
    }
  }

  def test_unless_direct_reference(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (a) ZIO.unit else ZIO.succeed(42)$END")).assertHighlighted()
      val text   = z(base(s"if (a) ZIO.unit else ZIO.succeed(42)"))
      val result = z(base(s"ZIO.succeed(42).unless(a)"))
      testQuickFixes(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.succeed(42) else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.succeed(42) else ZIO.unit"))
      val result = z(base(s"ZIO.succeed(42).unless(a)"))
      testQuickFixes(text, result, hint)
    }
  }

  def test_unless_direct_reference_apply(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (a) ZIO.unit else ZIO(42)$END")).assertHighlighted()
      val text   = z(base(s"if (a) ZIO.unit else ZIO(42)"))
      val result = z(base(s"ZIO(42).unless(a)"))
      testQuickFixes(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO(42) else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO(42) else ZIO.unit"))
      val result = z(base(s"ZIO(42).unless(a)"))
      testQuickFixes(text, result, hint)
    }
  }

  def test_unless_complex_reference(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |val list = (1 to 5).toList
          |val toZIO: Int => UIO[Int] = UIO.succeed(_)
          |$expr""".stripMargin
    val reference = "ZIO.foreach_(list)(toZIO)"

    locally {
      z(base(s"${START}if (a) ZIO.unit else $reference$END")).assertHighlighted()
      val text   = z(base(s"if (a) ZIO.unit else $reference"))
      val result = z(base(s"$reference.unless(a)"))
      testQuickFixes(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) $reference else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (!a) $reference else ZIO.unit"))
      val result = z(base(s"$reference.unless(a)"))
      testQuickFixes(text, result, hint)
    }
  }

  // ¯\_(ツ)_/¯
  def test_unless_with_multiple_negation(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |val b = ZIO.succeed(42)
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (!(!a)) ZIO.unit else b$END")).assertHighlighted()
      val text   = z(base(s"if (!(!a)) ZIO.unit else b"))
      val result = z(base(s"b.unless(a)"))
      testQuickFixes(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!(!(!a))) b else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (!(!(!a))) b else ZIO.unit"))
      val result = z(base(s"b.unless(a)"))
      testQuickFixes(text, result, hint)
    }
  }
}
