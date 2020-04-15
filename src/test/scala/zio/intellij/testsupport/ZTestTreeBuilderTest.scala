package zio.intellij.testsupport

import zio.intellij.testsupport.internal.ZTestTreeBuilder
import zio.intellij.testsupport.internal.ZTestTreeBuilder.Node.Kind
import zio.intellij.testsupport.internal.ZTestTreeBuilder.Node.Status.{ Completed, Failed }
import zio.intellij.testsupport.internal.ZTestTreeBuilder.TestTree.{ SuiteNode, TestNode }
import zio.test.Assertion._
import zio.test._

object ZTestTreeBuilderTest extends DefaultRunnableSpec {

  val spec = suite("builder")(
    test("successful run: Spec with nested spec, having a single successful test") {
      val out =
        """[32m+[0m ParserSpec
          |  [32m+[0m JSON
          |    [32m+[0m parse correctly
          |[36mRan 1 tests in 1 s 425 ms: 1 succeeded, 0 ignored, 0 failed[0m
          |""".stripMargin

      val builder = new ZTestTreeBuilder()
      builder.addAll(out)

      val expected =
        SuiteNode(
          "ParserSpec",
          List(
            SuiteNode(
              "JSON",
              List(
                TestNode("parse correctly", Completed, None)
              )
            )
          )
        )

      val actual = builder.testTree.get
      assert(actual)(equalTo(expected))
    },
    test("failed run: Spec with nested spec, having a single failed test due to assertion") {
      val out =
        """[31m-[0m ParserSpec
          |  [31m-[0m JSON
          |    [31m-[0m parse correctly
          |      [34mtrue[0m did not satisfy [36misFalse()[0m
          |[36mRan 1 tests in 1 s 425 ms: 0 succeeded, 0 ignored, 1 failed[0m
          |""".stripMargin

      val builder = new ZTestTreeBuilder()
      builder.addAll(out)

      val expected =
        SuiteNode(
          "ParserSpec",
          List(
            SuiteNode(
              "JSON",
              List(
                TestNode(
                  "parse correctly",
                  Failed(Kind.Assertion),
                  // due to a bug in fansi, it converts the reset [0m to [39m -|
                  Some("[34mtrue[39m did not satisfy [36misFalse()[39m") // |
                ) //                ^                                  ^       |
              )   //                +----------------------------------+-------+
            )
          )
        )

      val actual = builder.testTree.get
      assert(actual)(equalTo(expected))
    },
    test("failed run: failed with a crash") {
      // do NOT trim the 6 spaces between the messages
      val exceptionText =
        """      Fiber failed.
          |      An unchecked error was produced.
          |      scala.NotImplementedError: an implementation is missing
          |           at scala.Predef$.$qmark$qmark$qmark(Predef.scala:347)
          |           at ParserSpec$.$anonfun$spec$1(HelloSpec.scala:22)
          |           at zio.internal.FiberContext.evaluateNow(FiberContext.scala:382)
          |           at zio.internal.FiberContext.$anonfun$fork$2(FiberContext.scala:681)
          |           at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1149)
          |           at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:624)
          |           at java.lang.Thread.run(Thread.java:748)
          |
          |      Fiber:Id(1586909141999,11) was supposed to continue to:
          |        a future continuation at zio.test.package$ZTest$.apply(package.scala:104)
          |
          |      Fiber:Id(1586909141999,11) execution trace:
          |        at ParserSpec$.spec(HelloSpec.scala:22)
          |        at zio.ZIO$.effectSuspendTotal(ZIO.scala:2260)
          |""".stripMargin

      val out = s"""[31m-[0m ParserSpec
                   |  [31m-[0m JSON
                   |    [31m-[0m parse correctly
                   |$exceptionText
                   |[36mRan 1 tests in 1 s 425 ms: 0 succeeded, 0 ignored, 1 failed[0m
                   |""".stripMargin

      val builder = new ZTestTreeBuilder()
      builder.addAll(out)

      val expected =
        SuiteNode(
          "ParserSpec",
          List(
            SuiteNode(
              "JSON",
              List(
                TestNode("parse correctly", Failed(Kind.Error(exceptionText)), None)
              )
            )
          )
        )

      val actual = builder.testTree.get
      assert(actual)(equalTo(expected))
    }
  )

}
