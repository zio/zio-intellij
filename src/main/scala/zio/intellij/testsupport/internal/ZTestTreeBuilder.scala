package zio.intellij.testsupport.internal

import java.util.concurrent.atomic.AtomicReference

import fansi.{ Color, Str }
import zio.intellij.testsupport.internal.ZTestTreeBuilder.{ Graph, Node, TestTree }

import scala.annotation.tailrec

private[testsupport] class ZTestTreeBuilder {
  private val graph: AtomicReference[Graph] = new AtomicReference[Graph](Map.empty)

  def testTree: Option[TestTree] =
    for {
      ns   <- graph.get().get(0)
      head <- ns.headOption
    } yield head.toTestTree

  def addAll(lines: String): Unit =
    lines.split(System.lineSeparator()).foreach(addLine)

  def addLine(line: String): Unit =
    graph.getAndUpdate(g => add0(line)(g))

  private def add0(s: String)(graph: Graph): Graph = {
    @tailrec
    def updateParents(id: Int, node: Node, graph: Graph): Graph =
      graph.get(id) match {
        case None => graph
        case Some(nodes) =>
          nodes match {
            case Nil => throw new Exception("empty node")
            case _ =>
              val last  = nodes.last
              val child = last.find(node.name)
              val newChildren = child match {
                case None    => last.children :+ node
                case Some(c) => last.replace(c, node)
              }
              val newNode = last.copy(children = newChildren)
              updateParents(id - 1, newNode, graph.updated(id, nodes.init :+ newNode))
          }
      }

    val str  = s.dropWhile(_.isWhitespace)
    val id   = (s.length - str.length) / 2
    val node = Node(str)

    val g = graph.get(id) match {
      case None        => graph + (id -> Seq(node))
      case Some(nodes) => graph.updated(id, nodes :+ node)
    }
    updateParents(id - 1, node, g)
  }
}

object ZTestTreeBuilder {
  type Graph = Map[Int, Seq[Node]]

  sealed trait TestTree

  object TestTree {
    case class SuiteNode(label: String, specs: List[TestTree])                              extends TestTree
    case class TestNode(label: String, status: Node.Status, message: Option[String] = None) extends TestTree
  }

  case class Node(name: String, children: List[Node] = Nil) { self =>
    import Node.Kind._
    import Node.Status._
    import Node._
    import TestTree._

    private val ansi = Str(name)

    def find(s: String): Option[Node] = children.find(_.name == s)

    def replace(n: Node, r: Node): List[Node] = children.updated(children.indexOf(n), r)

    val kind: Node.Kind = self match {
      case IsAssertion()      => Assertion
      case IsSummary(summary) => summary
      case IsError(error)     => error
      case _                  => Kind.Unknown
    }

    def status: Node.Status = (ansi.getColor(0), ansi.getChar(0)) match {
      case (Color.Green.applyMask, '+') => Completed
      case (Color.Red.applyMask, '-')   => Failed(kind)
      case _                            => Status.Unknown
    }

    def assertString: Option[String] =
      if (children.isEmpty || !children.forall(_.isAssertion)) None
      else Some(children.map(_.name).mkString(System.lineSeparator()))

    val isAssertion: Boolean = kind match {
      case Assertion => true
      case _         => false
    }

    val isSummary: Boolean = kind match {
      case _: Summary => true
      case _          => false
    }

    def toTestTree: TestTree =
      assertString match {
        case a @ Some(_) => TestNode(plainText, Failed(Assertion), a)
        case None =>
          if (children.isEmpty) TestNode(plainText, Completed)
          else
            children.foldLeft(SuiteNode(plainText, List.empty)) { (suite, next) =>
              suite.copy(specs = suite.specs :+ next.toTestTree)
            }
      }

    private val plainText = (ansi.plainText, status) match {
      case (n, Status.Completed | Status.Failed(Assertion | Kind.Unknown)) => n.drop(2).trim
      case (n, _)                                                          => n
    }
  }

  object Node {
    val statusChars = List('+', '-')

    private def notStatus(c: Char, s: Char) =
      !statusChars.contains(c) && !s.isWhitespace

    object IsAssertion {

      def unapply(n: Node): Boolean =
        (n.ansi.getColor(0), n.ansi.getChar(0), n.ansi.getChar(1), n.status) match {
          case (Color.Blue.applyMask, c, s, Status.Unknown) if notStatus(c, s) => true
          case _                                                               => false
        }
    }

    object IsSummary {

      private val ran =
        "Ran (\\d) test(s)? in (.*(?=:)): (\\d).*(\\d).*(\\d).*".r

      def unapply(n: Node): Option[Kind.Summary] =
        (n.ansi.getColor(0), n.ansi.getChar(0), n.ansi.getChar(1)) match {
          case (Color.Cyan.applyMask, c, s) if notStatus(c, s) =>
            n.ansi.plainText match {
              case ran(totalTests, _, time, succeeded, ignored, failed) =>
                Some(Kind.Summary(totalTests, time, succeeded, ignored, failed))
              case _ => None
            }
          case _ => None
        }
    }

    object IsError {
      def unapply(n: Node): Option[Kind.Error] = None
      // todo handle stack traces
    }

    sealed trait Status

    object Status {
      case object Completed                extends Status
      case class Failed(failureKind: Kind) extends Status
      case object Unknown                  extends Status
    }
    sealed trait Kind

    object Kind {
      case class Error(ansi: Str) extends Kind
      case object Assertion       extends Kind

      case class Summary(
        totalTests: String,
        totalTime: String,
        succeeded: String,
        ignored: String,
        failed: String
      ) extends Kind
      case object Unknown extends Kind
    }
  }

}
