package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyServiceWithInspection

class SimplifyServiceWithInspectionTestZIO1 extends ZSimplifyInspectionTest[SimplifyServiceWithInspection] {
  override protected val hint = "Replace with ZIO.serviceWith"

  private def base(expr: String): String =
    s"""|trait Redis {
        |  def read: UIO[Unit]
        |  def read(n: Int): UIO[Unit]
        |  def readFromEnv: URIO[Has[Int], Unit]
        |}
        |type RedisService = Has[Redis]
        |$expr""".stripMargin

  def test_accessM_get_inferred(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Has[Redis], Unit] = $expr"
    val reference                = "ZIO.accessM(_.get.read)"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith(_.read)")))
    testQuickFix(text, result, hint)
  }

  def test_accessM_get_explicit_type(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Has[Redis], Unit] = $expr"
    val reference                = "ZIO.accessM[Has[Redis]](_.get.read)"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read)")))
    testQuickFix(text, result, hint)
  }

  def test_accessM_get_with_alias(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.accessM[RedisService](_.get.read)"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read)")))
    testQuickFix(text, result, hint)
  }

  def test_accessM_get_lambda(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.accessM[RedisService](x => x.get.read)"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read)")))
    testQuickFix(text, result, hint)
  }

  def test_accessM_get_method_call_with_param(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.accessM[RedisService](_.get.read(2))"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read(2))")))
    testQuickFix(text, result, hint)
  }

  def test_service_flatMap(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val reference                = "ZIO.service[Redis].flatMap(_.read(2))"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read(2))")))
    testQuickFix(text, result, hint)
  }

  def test_access_with_env_no_highlight(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService with Has[Int], Unit] = $expr"

    val reference = "ZIO.accessM[Has[Redis]](_.get.readFromEnv)"
    z(base(assignment(range(reference)))).assertNotHighlighted()
  }

  def test_service_with_env_no_highlight(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService with Has[Int], Unit] = $expr"
    val reference                = "ZIO.service[Redis].flatMap(_.readFromEnv)"
    z(base(assignment(range(reference)))).assertNotHighlighted()
  }

  def test_block_replacement(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"

    val reference = """ZIO.service[Redis].flatMap { redis =>
                      |  // random import
                      |  import zio._
                      |
                      |  for {
                      |    _ <- redis.read
                      |    _ <- redis.read(5)
                      |  } yield ()
                      |}""".stripMargin

    val simplified = """ZIO.serviceWith[Redis] {
                       |  redis =>
                       |    // random import
                       |    import zio._
                       |
                       |    for {
                       |      _ <- redis.read
                       |      _ <- redis.read(5)
                       |    } yield ()
                       |}""".stripMargin

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment(simplified)))
    testQuickFix(text, result, hint)
  }

  def test_simplify_block(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"

    val reference =
      """ZIO.accessM[RedisService] { x =>
        |  x.get.read
        |}""".stripMargin

    val simplified = "ZIO.serviceWith[Redis](_.read)"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment(simplified)))
    testQuickFix(text, result, hint)
  }

  def test_survive_incomplete_labmda(): Unit = {
    def assignment(expr: String) = s"val read: URIO[RedisService, Unit] = $expr"
    val incompleteExpr           = "ZIO.service[Redis].flatMap { redis => }"
    z(base(range(assignment(incompleteExpr)))).assertNotHighlighted() // just to trigger code analysis
  }

}

class SimplifyServiceWithInspectionTestZIO2 extends ZSimplifyInspectionTest[SimplifyServiceWithInspection] {
  override protected def isZIO1 = false

  override protected val hint = "Replace with ZIO.serviceWith"

  private def base(expr: String): String =
    s"""|trait Redis {
        |  def read(n: Int): Unit
        |  def readWithEnv: URIO[Int, Unit]
        |}
        |$expr""".stripMargin

  def test_service_map(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis, Unit] = $expr"
    val reference                = "ZIO.service[Redis].map(_.read(2))"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.read(2))")))
    testQuickFix(text, result, hint)
  }

  def test_service_with_env_map(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis, URIO[Int, Unit]] = $expr"

    val reference = "ZIO.service[Redis].map(_.readWithEnv)"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWith[Redis](_.readWithEnv)")))
    testQuickFix(text, result, hint)
  }

  def test_block_replacement(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis, Unit] = $expr"

    val reference = """ZIO.service[Redis].map { redis =>
                      |  // random import
                      |  import zio._
                      |
                      |  (redis.read(5), redis.read(5))
                      |}""".stripMargin

    val simplified = """ZIO.serviceWith[Redis] {
                       |  redis =>
                       |    // random import
                       |    import zio._
                       |
                       |    (redis.read(5), redis.read(5))
                       |}""".stripMargin

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment(simplified)))
    testQuickFix(text, result, hint)
  }

}

class SimplifyServiceWithZIOInspectionTestZIO2 extends ZSimplifyInspectionTest[SimplifyServiceWithInspection] {
  override protected def isZIO1 = false

  override protected val hint = "Replace with ZIO.serviceWithZIO"

  private def base(expr: String): String =
    s"""|trait Redis {
        |  def read: URIO[Int, Unit]
        |  def read(n: Int): UIO[Unit]
        |}
        |$expr""".stripMargin

  def test_service_flatMap(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis, Unit] = $expr"
    val reference                = "ZIO.service[Redis].flatMap(_.read(2))"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWithZIO[Redis](_.read(2))")))
    testQuickFix(text, result, hint)
  }

  def test_block_replacement(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis, Unit] = $expr"

    val reference = """ZIO.service[Redis].flatMap { redis =>
                      |  // random import
                      |  import zio._
                      |
                      |  for {
                      |    _ <- redis.read(5)
                      |    _ <- redis.read(5)
                      |  } yield ()
                      |}""".stripMargin

    val simplified = """ZIO.serviceWithZIO[Redis] {
                       |  redis =>
                       |    // random import
                       |    import zio._
                       |
                       |    for {
                       |      _ <- redis.read(5)
                       |      _ <- redis.read(5)
                       |    } yield ()
                       |}""".stripMargin

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment(simplified)))
    testQuickFix(text, result, hint)
  }

  def test_read_with_env_no_highlight(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis with Int, Unit] = $expr"
    val reference                = "ZIO.service[Redis].flatMap(_.read)"
    z(base(assignment(range(reference)))).assertNotHighlighted()
  }

  def test_recursive_env(): Unit = {
    def assignment(expr: String) = s"val read: URIO[Redis, Any] = $expr"
    val reference                = "ZIO.service[Redis].flatMap(_ => ZIO.service[Redis])"

    z(base(assignment(range(reference)))).assertHighlighted()

    val text   = z(base(assignment(reference)))
    val result = z(base(assignment("ZIO.serviceWithZIO[Redis](_ => ZIO.service[Redis])")))
    testQuickFix(text, result, hint)
  }

}
