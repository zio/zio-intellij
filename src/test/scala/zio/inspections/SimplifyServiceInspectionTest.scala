package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyServiceInspection

class SimplifyServiceInspectionTest extends ZSimplifyInspectionTest[SimplifyServiceInspection] {
  override protected val hint = "Replace with ZIO.service"

  private def base(expr: String): String =
    s"""|type UserId = String
        |final case class User(id: UserId, name: String)
        |
        |type UserRepo = Has[UserRepo.Service]
        |type AnotherAlias = UserRepo
        |type AndOneMoreAlias = AnotherAlias
        |object UserRepo {
        |  trait Service {
        |    def getUser(userId: UserId): Task[Option[User]]
        |    def createUser(user: User): Task[Unit]
        |    def notEffect: Boolean
        |  }
        |  val testRepo: ULayer[UserRepo] = ZLayer.succeed(???)
        |}
        |
        |$expr""".stripMargin

  def test_access_get(): Unit = {
    def assignment(expr: String) = s"val res: URIO[Has[UserRepo.Service], UserRepo.Service] = $expr"
    val reference                = "RIO.access(_.get)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("RIO.service")))
    testQuickFix(text, result, hint)
  }

  def test_access_get_with_alias(): Unit = {
    def assignment(expr: String) = s"val res: URIO[UserRepo, UserRepo.Service] = $expr"
    val reference                = "URIO.access(_.get)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("URIO.service")))
    testQuickFix(text, result, hint)
  }

  def test_access_get_lambda(): Unit = {
    def assignment(expr: String) = s"val res: URIO[UserRepo, UserRepo.Service] = $expr"
    val reference                = "ZIO.access(h => h.get)"

    z(base(assignment(s"$START$reference$END")))
      .assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.service")))
    testQuickFix(text, result, hint)
  }

  def test_not_highlighted(): Unit = {
    def assignment(expr: String) = s"val res: URIO[Has[UserRepo.Service], Boolean] = $expr"
    val reference                = "ZIO.access(_.get.notEffect)"

    z(base(assignment(s"$START$reference$END")))
      .assertNotHighlighted()
  }

  def test_generic_access_get(): Unit = {
    val reference = "ZIO.access[UserRepo](_.get)"

    z(base(s"$START$reference$END"))
      .assertHighlighted()

    val text   = z(base(reference))
    val result = z(base("ZIO.service[UserRepo.Service]"))
    testQuickFix(text, result, hint)
  }

  def test_generic_access_get_lambda(): Unit = {
    val reference = "ZIO.access[UserRepo](h => h.get)"

    z(base(s"$START$reference$END"))
      .assertHighlighted()

    val text   = z(base(reference))
    val result = z(base("ZIO.service[UserRepo.Service]"))
    testQuickFix(text, result, hint)
  }

  def test_generic_not_highlighted(): Unit = {
    val reference = "ZIO.access[UserRepo](_.get.notEffect)"

    z(base(s"$START$reference$END"))
      .assertNotHighlighted()
  }

  def test_multiple_aliases(): Unit = {
    val reference = "ZIO.access[AndOneMoreAlias](_.get)"

    z(base(s"$START$reference$END"))
      .assertHighlighted()

    val text   = z(base(reference))
    val result = z(base("ZIO.service[UserRepo.Service]"))
    testQuickFix(text, result, hint)
  }

}
