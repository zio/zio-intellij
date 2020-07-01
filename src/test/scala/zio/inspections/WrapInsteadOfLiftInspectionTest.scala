package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.mistakes.WrapInsteadOfLiftInspection

abstract class BaseWrapInsteadOfLiftInspectionTest(s: String)
    extends ZScalaInspectionTest[WrapInsteadOfLiftInspection] {
  override protected val description = WrapInsteadOfLiftInspection.messageFormat.format(s, s)
}

class OptionWrapInspectionTest extends BaseWrapInsteadOfLiftInspectionTest("Option") {

  val hint = "Replace with ZIO.fromOption"

  def test_option_reference_ctor(): Unit = {
    z(s"""val o: Option[Int] = Option(42)
         |${START}ZIO(o)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val o: Option[Int] = Option(42)
                    |ZIO(o)""".stripMargin)
    val result = z(s"""val o: Option[Int] = Option(42)
                      |ZIO.fromOption(o)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_option_reference_apply(): Unit = {
    z(s"""val o: Option[Int] = Option(42)
         |${START}ZIO.apply(o)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val o: Option[Int] = Option(42)
                    |ZIO.apply(o)""".stripMargin)
    val result = z(s"""val o: Option[Int] = Option(42)
                      |ZIO.fromOption(o)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_option_direct_some(): Unit = {
    z(s"${START}ZIO(Some(42))$END").assertHighlighted()
    val text   = z(s"ZIO(Some(42))")
    val result = z(s"ZIO.fromOption(Some(42))")
    testQuickFix(text, result, hint)
  }

  def test_option_direct_none(): Unit = {
    z(s"${START}ZIO(None)$END").assertHighlighted()
    val text   = z(s"ZIO(None)")
    val result = z(s"ZIO.fromOption(None)")
    testQuickFix(text, result, hint)
  }

  def test_option_effect(): Unit = {
    z(s"${START}ZIO.effect(Option(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effect(Option(42))")
    val result = z(s"ZIO.fromOption(Option(42))")
    testQuickFix(text, result, hint)
  }

  def test_option_effectTotal(): Unit = {
    z(s"${START}ZIO.effectTotal(Option(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effectTotal(Option(42))")
    val result = z(s"ZIO.fromOption(Option(42))")
    testQuickFix(text, result, hint)
  }

  def test_non_option_getOrElse(): Unit =
    z(s"""val o: Option[String] = ???
         |${START}ZIO.effect(o.getOrElse(""))$END""".stripMargin).assertNotHighlighted()

  def test_option_map(): Unit = {
    z(s"""val o: Option[String] = ???
         |${START}ZIO.effect(o.map(_ + " foo"))$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val o: Option[String] = ???
                    |ZIO.effect(o.map(_ + " foo"))""".stripMargin)
    val result = z(s"""val o: Option[String] = ???
                      |ZIO.fromOption(o.map(_ + " foo"))""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_nested(): Unit = {
    z(s"${START}ZIO.effect(util.Right(util.Try(Option(42))).getOrElse(util.Try(None)).getOrElse(None))$END")
      .assertHighlighted()

    val text   = z("ZIO.effect(util.Right(util.Try(Option(42))).getOrElse(util.Try(None)).getOrElse(None))")
    val result = z("ZIO.fromOption(util.Right(util.Try(Option(42))).getOrElse(util.Try(None)).getOrElse(None))")
    testQuickFix(text, result, hint)
  }
}

class TryWrapInspectionTest extends BaseWrapInsteadOfLiftInspectionTest("Try") {

  val hint = "Replace with ZIO.fromTry"

  def test_try_reference_ctor(): Unit = {
    z(s"""val t: Try[Int] = Try(42)
         |${START}ZIO(t)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val t: Try[Int] = Try(42)
                    |ZIO(t)""".stripMargin)
    val result = z(s"""val t: Try[Int] = Try(42)
                      |ZIO.fromTry(t)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_try_reference_apply(): Unit = {
    z(s"""val t: Try[Int] = Try(42)
         |${START}ZIO.apply(t)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val t: Try[Int] = Try(42)
                    |ZIO.apply(t)""".stripMargin)
    val result = z(s"""val t: Try[Int] = Try(42)
                      |ZIO.fromTry(t)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_try_direct_success(): Unit = {
    z(s"${START}ZIO(Success(42))$END").assertHighlighted()
    val text   = z(s"ZIO(Success(42))")
    val result = z(s"ZIO.fromTry(Success(42))")
    testQuickFix(text, result, hint)
  }

  def test_try_direct_failure(): Unit = {
    z(s"${START}ZIO(Failure(new Exception()))$END").assertHighlighted()
    val text   = z(s"ZIO(Failure(new Exception()))")
    val result = z(s"ZIO.fromTry(Failure(new Exception()))")
    testQuickFix(text, result, hint)
  }

  def test_try_effect(): Unit = {
    z(s"${START}ZIO.effect(Try(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effect(Try(42))")
    val result = z(s"ZIO.fromTry(Try(42))")
    testQuickFix(text, result, hint)
  }

  def test_try_effectTotal(): Unit = {
    z(s"${START}ZIO.effectTotal(Try(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effectTotal(Try(42))")
    val result = z(s"ZIO.fromTry(Try(42))")
    testQuickFix(text, result, hint)
  }

  def test_nested(): Unit = {
    z(s"${START}ZIO.effect(util.Right(util.Try(Option(42))).getOrElse(util.Try(None)))$END")
      .assertHighlighted()

    val text   = z("ZIO.effect(util.Right(util.Try(Option(42))).getOrElse(util.Try(None)))")
    val result = z("ZIO.fromTry(util.Right(util.Try(Option(42))).getOrElse(util.Try(None)))")
    testQuickFix(text, result, hint)
  }
}

class EitherWrapInspectionTest extends BaseWrapInsteadOfLiftInspectionTest("Either") {

  val hint = "Replace with ZIO.fromEither"

  def test_either_reference_ctor(): Unit = {
    z(s"""val either: Either[String, Int] = Right(42)
         |${START}ZIO(either)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val either: Either[String, Int] = Right(42)
                    |ZIO(either)""".stripMargin)
    val result = z(s"""val either: Either[String, Int] = Right(42)
                      |ZIO.fromEither(either)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_either_reference_apply(): Unit = {
    z(s"""val either: Either[String, Int] = Right(42)
         |${START}ZIO.apply(either)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val either: Either[String, Int] = Right(42)
                    |ZIO.apply(either)""".stripMargin)
    val result = z(s"""val either: Either[String, Int] = Right(42)
                      |ZIO.fromEither(either)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_either_direct_right(): Unit = {
    z(s"${START}ZIO(Right(42))$END").assertHighlighted()
    val text   = z(s"ZIO(Right(42))")
    val result = z(s"ZIO.fromEither(Right(42))")
    testQuickFix(text, result, hint)
  }

  def test_either_direct_left(): Unit = {
    z(s"${START}ZIO(Left(42))$END").assertHighlighted()
    val text   = z(s"ZIO(Left(42))")
    val result = z(s"ZIO.fromEither(Left(42))")
    testQuickFix(text, result, hint)
  }

  def test_either_effect(): Unit = {
    z(s"${START}ZIO.effect(Right(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effect(Right(42))")
    val result = z(s"ZIO.fromEither(Right(42))")
    testQuickFix(text, result, hint)
  }

  def test_either_effectTotal(): Unit = {
    z(s"${START}ZIO.effectTotal(Right(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effectTotal(Right(42))")
    val result = z(s"ZIO.fromEither(Right(42))")
    testQuickFix(text, result, hint)
  }

  def test_nested(): Unit = {
    z(s"${START}ZIO.effect(util.Right(util.Try(Option(42))))$END")
      .assertHighlighted()

    val text   = z("ZIO.effect(util.Right(util.Try(Option(42))))")
    val result = z("ZIO.fromEither(util.Right(util.Try(Option(42))))")
    testQuickFix(text, result, hint)
  }
}

class FutureWrapInspectionTest extends BaseWrapInsteadOfLiftInspectionTest("Future") {

  val hint = "Replace with ZIO.fromFuture"

  def test_future_reference_ctor(): Unit = {
    z(s"""val future = Future(42)
         |${START}ZIO(future)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val future = Future(42)
                    |ZIO(future)""".stripMargin)
    val result = z(s"""val future = Future(42)
                      |ZIO.fromFuture(implicit ec => future)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_future_reference_apply(): Unit = {
    z(s"""val future = Future(42)
         |${START}ZIO.apply(future)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val future = Future(42)
                    |ZIO.apply(future)""".stripMargin)
    val result = z(s"""val future = Future(42)
                      |ZIO.fromFuture(implicit ec => future)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_future_direct_method(): Unit = {
    z(s"${START}ZIO(Future(42))$END").assertHighlighted()
    val text   = z(s"ZIO(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }

  def test_future_effect(): Unit = {
    z(s"${START}ZIO.effect(Future(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effect(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }

  def test_future_effectTotal(): Unit = {
    z(s"${START}ZIO.effectTotal(Future(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effectTotal(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }

  def test_zio_alias_task(): Unit = {
    z(s"${START}Task.effectTotal(Future(42))$END").assertHighlighted()
    val text   = z(s"Task.effectTotal(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }
}
