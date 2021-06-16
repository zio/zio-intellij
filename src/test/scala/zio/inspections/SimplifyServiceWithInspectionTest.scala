package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyServiceWithInspection

class SimplifyServiceWithInspectionTest extends ZSimplifyInspectionTest[SimplifyServiceWithInspection] {
  override protected val hint = "Replace with ZIO.serviceWith"

  private def base(expr: String): String =
    s"""|trait Redis {
        |  def read: UIO[Unit]
        |  def read(n: Int): UIO[Unit]
        |}
        |type RedisService = Has[Redis]
        |$expr""".stripMargin

  def test_accessM_get_inferred(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Has[Redis], Unit] = $expr"
    val reference                = "ZIO.accessM(_.get.read)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith(_.read)")))
    testQuickFixes(text, result, hint)
  }

  def test_accessM_get_explicit_type(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Has[Redis], Unit] = $expr"
    val reference                = "ZIO.accessM[Has[Redis]](_.get.read)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read)")))
    testQuickFixes(text, result, hint)
  }

  def test_accessM_get_with_alias(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.accessM[RedisService](_.get.read)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read)")))
    testQuickFixes(text, result, hint)
  }

  def test_accessM_get_lambda(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.accessM[RedisService](x => x.get.read)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read)")))
    testQuickFixes(text, result, hint)
  }

  def test_accessM_get_method_call_with_param(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.accessM[RedisService](_.get.read(2))"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read(2))")))
    testQuickFixes(text, result, hint)
  }

  def test_service_flatMap(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.service[Redis].flatMap(_.read(2))"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read(2))")))
    testQuickFixes(text, result, hint)
  }
}
