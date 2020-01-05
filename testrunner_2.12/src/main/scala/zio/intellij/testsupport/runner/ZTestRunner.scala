package zio.intellij.testsupport.runner

import zio.duration.Duration
import zio.test._
import zio.{Ref, UIO, ZIO}

object ZTestRunner {
  case class Args private (testClass: String, testMethod: Option[String] = None)
  object Args {
    def parse(args: List[String]): Args = {
      // [-s testClassName ...] [-t testMethodName ...] -showProgressMessages bool
      @scala.annotation.tailrec
      def go(args: List[String], parsed: Args): Args = args match {
        case "-s" :: testClass :: xs         => go(xs, parsed.copy(testClass = testClass))
        case "-testName" :: testMethod :: xs => go(xs, parsed.copy(testMethod = Some(testMethod)))
        case _                               => parsed
      }
      go(args, Args(""))
    }
  }

  def createSpec(args: Args): AbstractRunnableSpec = {
    import org.portablescala.reflect._
    val fqn = args.testClass.stripSuffix("$") + "$"
    Reflect
      .lookupLoadableModuleClass(fqn, classOf[ZTestRunnerHost].getClassLoader)
      .getOrElse(throw new ClassNotFoundException("failed to load object: " + fqn))
      .loadModule()
      .asInstanceOf[AbstractRunnableSpec]

  }

  def run(args: Array[String]): Unit = {
    val parsedArgs = Args.parse(args.toList)
    val spec       = createSpec(parsedArgs)
    spec.runner
      .withReporter(TestRunnerReporter[spec.Label, spec.Failure, spec.Success]())
      .unsafeRun {
        parsedArgs.testMethod.fold(spec.spec)(method => {
          spec.spec
            .filterLabels(_.toString.contains(method))
            .getOrElse(spec.spec)
        })
      }
  }
}

object TestRunnerReporter {
  def apply[L, E, S](): TestReporter[L, E, S] = { (_: Duration, executedSpec: ExecutedSpec[L, E, S]) =>
    for {
      res <- render(executedSpec.mapLabel(_.toString))
      _   <- ZIO.foreach(res)(s => ZIO.effectTotal(println(s)))
    } yield ()
  }

  def render[E, S](executedSpec: ExecutedSpec[String, E, S]): UIO[Seq[String]] =
    Ref.make(0).flatMap { idCounter =>
      def newId: UIO[Int] =
        idCounter.update(_ + 1)

      def loop(executedSpec: ExecutedSpec[String, E, S], pid: Int): UIO[Seq[String]] =
        executedSpec.caseValue match {
          case Spec.SuiteCase(label, executedSpecs, _) =>
            for {
              id       <- newId
              specs    <- executedSpecs
              started  = suiteStarted(label, id, pid)
              finished = suiteFinished(label, id)
              rest     <- UIO.foreach(specs)(loop(_, id)).map(_.flatten)
            } yield started +: rest :+ finished
          case Spec.TestCase(label, result) =>
            for {
              id      <- newId
              results <- DefaultTestReporter.render(executedSpec.mapLabel(_.toString))
              started = testStarted(label, id, pid)
              finished <- result.map {
                           case Right(TestSuccess.Succeeded(_)) =>
                             Seq(testFinished(label, id))
                           case Right(TestSuccess.Ignored) =>
                             Seq(testIgnored(label, id))
                           case Left(_) =>
                             Seq(testFailed(label, id, results.toList))
                         }
            } yield started +: finished
        }
      loop(executedSpec, 0)
    }

  private def suiteStarted(label: String, id: Int, parentId: Int) =
    tc(s"testSuiteStarted name='${escapeString(label)}' nodeId='$id' parentNodeId='$parentId' " +
      s"captureStandardOutput='true'")

  private def suiteFinished(label: String, id: Int) =
    tc(s"testSuiteFinished name='${escapeString(label)}' nodeId='$id'")

  private def testStarted(label: String, id: Int, parentId: Int) =
    tc(s"testStarted name='${escapeString(label)}' nodeId='$id' parentNodeId='$parentId' " +
      s"captureStandardOutput='true'")

  private def testFinished(label: String, id: Int) =
    tc(s"testFinished name='${escapeString(label)}' nodeId='$id'")

  private def testIgnored(label: String, id: Int) =
    tc(s"testIgnored name='${escapeString(label)}' nodeId='$id'")

  private def testFailed(label: String, id: Int, res: List[RenderedResult]) = res match {
    case r :: Nil =>
      tc(s"testFailed name='${escapeString(label)}' nodeId='$id' message='Assertion failed:' " +
          s"details='${escapeString(r.rendered.drop(1).mkString("\n"))}'")
    case _ => tc(s"testFailed name='${escapeString(label)}' message='Assertion failed' nodeId='$id'")
  }

  def tc(message: String): String =
    s"##teamcity[$message]"

  def escapeString(str: String): String =
    str
      .replaceAll("[|]", "||")
      .replaceAll("[']", "|'")
      .replaceAll("[\n]", "|n")
      .replaceAll("[\r]", "|r")
      .replaceAll("]", "|]")
      .replaceAll("\\[", "|[")
}
