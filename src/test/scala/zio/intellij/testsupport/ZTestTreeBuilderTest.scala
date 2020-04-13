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

      assert(builder.testTree)(isSome(equalTo(expected)))
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
                  Some("[34mtrue[0m did not satisfy [36misFalse()[0m")
                )
              )
            )
          )
        )

      val actual = builder.testTree.get
      assert(actual)(equalTo(expected))
    }
  )

}
