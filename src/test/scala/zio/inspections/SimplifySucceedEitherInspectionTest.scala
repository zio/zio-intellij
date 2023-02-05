package zio.inspections

import zio.intellij.inspections.simplifications.SimplifySucceedEitherInspection

abstract class SimplifySucceedEitherInspectionTest(s: String)
    extends ZSimplifyInspectionTest[SimplifySucceedEitherInspection] {
  override protected val hint = s"Replace with $s"
}

class SucceedLeftInspectionTest extends SimplifySucceedEitherInspectionTest("ZIO.left") {

  def test_succeed_Left(): Unit = {
    z(s"${START}ZIO.succeed(Left(a))$END").assertHighlighted()
    val text   = z("ZIO.succeed(Left(a))")
    val result = z("ZIO.left(a)")
    testQuickFix(text, result, hint)
  }

  def test_block_succeed_Left(): Unit = {
    z {
      s"""${START}ZIO.succeed {
         |  Left {
         |    a
         |    a
         |    a
         |  }
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed {
        |  Left {
        |    a
        |    a
        |    a
        |  }
        |}""".stripMargin
    }
    val result = z {
      """ZIO.left {
        |  a
        |  a
        |  a
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_succeed_util_Left(): Unit = {
    z(s"${START}ZIO.succeed(util.Left(a))$END").assertHighlighted()
    val text   = z("ZIO.succeed(util.Left(a))")
    val result = z("ZIO.left(a)")
    testQuickFix(text, result, hint)
  }

  def test_block_succeed_util_Left(): Unit = {
    z {
      s"""${START}ZIO.succeed {
         |  util.Left {
         |    a
         |    a
         |    a
         |  }
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed {
        | util.Left {
        |    a
        |    a
        |    a
        |  }
        |}""".stripMargin
    }
    val result = z {
      """ZIO.left {
        |  a
        |  a
        |  a
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_UIO_Left(): Unit = {
    z(s"${START}UIO(Left(a))$END").assertHighlighted()
    val text   = z("UIO(Left(a))")
    val result = z("UIO.left(a)")
    testQuickFix(text, result, hint)
  }

  def test_UIO_util_Left(): Unit = {
    z(s"${START}UIO(util.Left(a))$END").assertHighlighted()
    val text   = z("UIO(util.Left(a))")
    val result = z("UIO.left(a)")
    testQuickFix(text, result, hint)
  }

  def test_UIO_apply_Left(): Unit = {
    z(s"${START}UIO.apply(Left(a))$END").assertHighlighted()
    val text   = z("UIO.apply(Left(a))")
    val result = z("UIO.left(a)")
    testQuickFix(text, result, hint)
  }

  def test_UIO_apply_util_Left(): Unit = {
    z(s"${START}UIO.apply(util.Left(a))$END").assertHighlighted()
    val text   = z("UIO.apply(util.Left(a))")
    val result = z("UIO.left(a)")
    testQuickFix(text, result, hint)
  }
}

class SucceedRightInspectionTest extends SimplifySucceedEitherInspectionTest("ZIO.right") {

  def test_succeed_Right(): Unit = {
    z(s"${START}ZIO.succeed(Right(a))$END").assertHighlighted()
    val text   = z("ZIO.succeed(Right(a))")
    val result = z("ZIO.right(a)")
    testQuickFix(text, result, hint)
  }

  def test_block_succeed_Right(): Unit = {
    z {
      s"""${START}ZIO.succeed {
         |  Right {
         |    a
         |    a
         |    a
         |  }
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed {
        |  Right {
        |    a
        |    a
        |    a
        |  }
        |}""".stripMargin
    }
    val result = z {
      """ZIO.right {
        |  a
        |  a
        |  a
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_succeed_util_Right(): Unit = {
    z(s"${START}ZIO.succeed(util.Right(a))$END").assertHighlighted()
    val text   = z("ZIO.succeed(util.Right(a))")
    val result = z("ZIO.right(a)")
    testQuickFix(text, result, hint)
  }

  def test_block_succeed_util_Right(): Unit = {
    z {
      s"""${START}ZIO.succeed {
         |  util.Right {
         |    a
         |    a
         |    a
         |  }
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed {
        | util.Right {
        |    a
        |    a
        |    a
        |  }
        |}""".stripMargin
    }
    val result = z {
      """ZIO.right {
        |  a
        |  a
        |  a
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_UIO_Right(): Unit = {
    z(s"${START}UIO(Right(a))$END").assertHighlighted()
    val text   = z("UIO(Right(a))")
    val result = z("UIO.right(a)")
    testQuickFix(text, result, hint)
  }

  def test_UIO_util_Right(): Unit = {
    z(s"${START}UIO(util.Right(a))$END").assertHighlighted()
    val text   = z("UIO(util.Right(a))")
    val result = z("UIO.right(a)")
    testQuickFix(text, result, hint)
  }

  def test_UIO_apply_Right(): Unit = {
    z(s"${START}UIO.apply(Right(a))$END").assertHighlighted()
    val text   = z("UIO.apply(Right(a))")
    val result = z("UIO.right(a)")
    testQuickFix(text, result, hint)
  }

  def test_UIO_apply_util_Right(): Unit = {
    z(s"${START}UIO.apply(util.Right(a))$END").assertHighlighted()
    val text   = z("UIO.apply(util.Right(a))")
    val result = z("UIO.right(a)")
    testQuickFix(text, result, hint)
  }
}
