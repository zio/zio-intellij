package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyBimapInspection

class SimplifyBimapInspectionTest extends ZSimplifyInspectionTest[SimplifyBimapInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .bimap"

  def test_map_mapError(): Unit = {
    z(s"ZIO.succeed(42).${START}map(a).mapError(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(a).mapError(b)")
    val result = z("ZIO.succeed(42).bimap(b, a)")
    testQuickFixes(text, result, hint)
  }

  def test_block_map_mapError(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}map { a =>
         |  a
         |  a
         |  a
         |}
         |.mapError { b =>
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).map { a =>
        |  a
        |  a
        |  a
        |}.mapError { b =>
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  b =>
        |    b
        |    b
        |    b
        |  }, {
        |  a =>
        |    a
        |    a
        |    a
        |  })""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_map_orElseFail(): Unit = {
    z(s"ZIO.succeed(42).${START}map(a).orElseFail(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(a).orElseFail(b)")
    val result = z("ZIO.succeed(42).bimap(_ => b, a)")
    testQuickFixes(text, result, hint)
  }

  def test_block_map_orElseFail(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}map { a =>
         |  a
         |  a
         |  a
         |}.orElseFail {
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).map { a =>
        |  a
        |  a
        |  a
        |}.orElseFail {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  _ => {
        |      b
        |      b
        |      b
        |    }
        |}, {
        |  a =>
        |    a
        |    a
        |    a
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_as_mapError(): Unit = {
    z(s"ZIO.succeed(42).${START}as(a).mapError(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).as(a).mapError(b)")
    val result = z("ZIO.succeed(42).bimap(b, _ => a)")
    testQuickFixes(text, result, hint)
  }

  def test_block_as_mapError(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}as {
         |  a
         |  a
         |  a
         |}.mapError{
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).as {
        |  a
        |  a
        |  a
        |}.mapError {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  b
        |  b
        |  b
        |}, {
        |  _ => {
        |    a
        |    a
        |    a
        |  }
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_as_orElseFail(): Unit = {
    z(s"ZIO.succeed(42).${START}as(a).orElseFail(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).as(a).orElseFail(b)")
    val result = z("ZIO.succeed(42).bimap(_ => b, _ => a)")
    testQuickFixes(text, result, hint)
  }

  def test_block_as_orElseFail(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}as {
         |  a
         |  a
         |  a
         |}.orElseFail {
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).as {
        |  a
        |  a
        |  a
        |}.orElseFail {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  _ => {
        |    b
        |    b
        |    b
        |  }
        |}, {
        |  _ => {
        |      a
        |      a
        |      a
        |    }
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_mapError_map(): Unit = {
    z(s"ZIO.succeed(42).${START}mapError(a).map(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).mapError(a).map(b)")
    val result = z("ZIO.succeed(42).bimap(a, b)")
    testQuickFixes(text, result, hint)
  }

  def test_block_mapError_map(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}mapError {
         |  a
         |  a
         |  a
         |}.map {
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).mapError {
        |  a
        |  a
        |  a
        |}.map {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  a
        |  a
        |  a
        |}, {
        |  b
        |  b
        |  b
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_mapError_as(): Unit = {
    z(s"ZIO.succeed(42).${START}mapError(a).as(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).mapError(a).as(b)")
    val result = z("ZIO.succeed(42).bimap(a, _ => b)")
    testQuickFixes(text, result, hint)
  }

  def test_block_mapError_as(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}mapError {
         |  a
         |  a
         |  a
         |}.as {
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).mapError {
        |  a
        |  a
        |  a
        |}.as {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  a
        |  a
        |  a
        |}, {
        |  _ => {
        |      b
        |      b
        |      b
        |    }
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_orElseFail_map(): Unit = {
    z(s"ZIO.succeed(42).${START}orElseFail(a).map(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).orElseFail(a).map(b)")
    val result = z("ZIO.succeed(42).bimap(_ => a, b)")
    testQuickFixes(text, result, hint)
  }

  def test_block_orElseFail_map(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}orElseFail {
         |  a
         |  a
         |  a
         |}.map {
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).orElseFail {
        |  a
        |  a
        |  a
        |}.map {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        | _ => {
        |    a
        |    a
        |    a
        |  }
        |}, {
        |  b
        |  b
        |  b
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_orElseFail_as(): Unit = {
    z(s"ZIO.succeed(42).${START}orElseFail(a).as(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).orElseFail(a).as(b)")
    val result = z("ZIO.succeed(42).bimap(_ => a, _ => b)")
    testQuickFixes(text, result, hint)
  }

  def test_block_orElseFail_as(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}orElseFail {
         |  a
         |  a
         |  a
         |}.as {
         |  b
         |  b
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()

    val text = z {
      """ZIO.succeed(42).orElseFail {
        |  a
        |  a
        |  a
        |}.as {
        |  b
        |  b
        |  b
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).bimap({
        |  _ => {
        |      a
        |      a
        |      a
        |    }
        |}, {
        |  _ => {
        |      b
        |      b
        |      b
        |    }
        |})""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }
}
