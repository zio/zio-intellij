package zio.intellij.inspections.macros

import com.intellij.codeInspection._
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.{
  createExpressionFromText,
  createTypeElementFromText
}
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{api, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.project.{ProjectContext, ProjectPsiElementExt, ScalaFeatures}
import zio.intellij.inspections.macros.LayerBuilder._
import zio.intellij.inspections.macros.LayerTree.{ComposeH, ComposeV, Empty, Value}
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._

import java.lang.System.lineSeparator

class ProvideMacroZIO2Inspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case expr @ `.provide`(Typeable(`ZIO[R, E, A]`(r, _, _)), layers @ _*) if expr.module.exists(_.isZio2) =>
      implicit val tpContext: TypePresentationContext = expr
      implicit val pContext: ProjectContext           = expr
      implicit val scalaFeatures: ScalaFeatures       = expr
      val result =
        LayerBuilder(
          target0 = split(r).map(new ZType(_)).toList,
          remainder = List.empty,
          providedLayers = layers.map(new ZExpr(_)).toList,
          method = ProvideMethod.Provide
        ).tryBuild

      result match {
        case Left(issue) => visitIssue(holder, expr)(issue)
        case Right(_)    =>
      }
    case _ =>
  }

  private def visitIssue(holder: ProblemsHolder, expr: ScExpression)(issue: ConstructionIssue): Unit =
    issue match {
      case error: ConstructionError     => visitError(holder, expr)(error)
      case warning: ConstructionWarning => visitWarning(holder, expr)(warning)
    }

  import ErrorRendering._

  private def visitError(holder: ProblemsHolder, expr: ScExpression)(error: ConstructionError): Unit =
    error match {
      case MissingLayersError(topLevel, transitives) =>
        holder.registerProblem(
          expr,
          missingLayersError(topLevel, transitives, isUsingProvideSome = false),
          ProblemHighlightType.GENERIC_ERROR
        )
      case DuplicateLayersError(duplicates) =>
        duplicates.foreach {
          case (tpe, exprs) =>
            exprs.foreach { expr =>
              holder.registerProblem(expr.value, ambigousLayersError(tpe, exprs), ProblemHighlightType.GENERIC_ERROR)
            }
        }
      case CircularityError(circular) =>
        circular.foreach {
          case (a, b) =>
            holder.registerProblem(expr, circularityError(a, b), ProblemHighlightType.GENERIC_ERROR)
        }
    }

  private def visitWarning(holder: ProblemsHolder, expr: ScExpression)(warning: ConstructionWarning): Unit =
    warning match {
      case UnusedLayersWarning(layers) =>
        layers.foreach { layer =>
          holder.registerProblem(layer.value, unusedLayersWarning, ProblemHighlightType.WEAK_WARNING)
        }
      case UnusedProvideSomeLayersWarning(layers) =>
        holder.registerProblem(expr, unusedProvideSomeLayersWarning(layers), ProblemHighlightType.WEAK_WARNING)
      case SuperfluousProvideCustomWarning =>
        holder.registerProblem(expr, superfluousProvideCustomWarning, ProblemHighlightType.WEAK_WARNING)
      case ProvideSomeAnyEnvWarning =>
        holder.registerProblem(expr, provideSomeAnyEnvWarning, ProblemHighlightType.WEAK_WARNING)
      case Warnings(w) =>
        w.foreach(visitWarning(holder, expr))
    }

}

/**
 * LayerBuilder houses the core logic for compile-time layer construction
 *
 * @param target0
 *   A list of types indicating the intended output of the final layer. This is
 *   generally determined by the `R` of the effect that ZIO.provide is
 *   called on.
 * @param remainder
 *   A list of types indicating the input of the final layer. This would be the
 *   parameter of ZIO.provideSome
 * @param providedLayers
 *   A list of layers that have been provided by the user.
 * @param method
 *   The sort of method that is being called: `provide`, `provideSome`, or
 *   `provideCustom`. This is used to provide improved warnings.
 */
final case class LayerBuilder(
  target0: List[ZType],
  remainder: List[ZType],
  providedLayers: List[ZExpr],
  method: ProvideMethod
)(implicit tpContext: TypePresentationContext, pContext: ProjectContext, scalaFeatures: ScalaFeatures) {

  private val anyStdType      = api.Any
  private val debugLayerType  = createTypeElementFromText("_root_.zio.ZLayer.Debug", scalaFeatures).`type`().toOption
  private val sideEffectZType = new ZType(api.Unit)

  private lazy val target = {
    val target1 =
      if (method.isProvideSomeShared) target0.filterNot(t1 => remainder.exists(t2 => t2.conforms(t1)))
      else target0
    target1.filterNot(_.value.equiv(anyStdType))
  }

  private lazy val remainderNodes: List[Node] = remainder.map(typeToNode).distinct

  private val (sideEffectNodes, providedLayerNodes): (List[Node], List[Node]) =
    providedLayers.flatMap(layerToNode).partition(_.outputs.exists(sideEffectZType.conforms))

  def tryBuild: Either[ConstructionIssue, Unit] =
    assertNoAmbiguity.flatMap(_ => tryBuildInternal)

  /**
   * Checks to see if any type is provided by multiple layers.
   */
  private def assertNoAmbiguity: Either[DuplicateLayersError, Unit] = {
    val typesToExprs =
      providedLayerNodes.flatMap { node =>
        node.outputs.map(output => output -> node.value)
      }.groupMap(_._1)(_._2)

    val duplicates = typesToExprs.filter { case (_, list) => list.size > 1 }

    if (duplicates.isEmpty) Right(())
    else Left(DuplicateLayersError(duplicates))
  }

  private def tryBuildInternal: Either[ConstructionIssue, Unit] = {

    /**
     * Build the layer tree. This represents the structure of a successfully
     * constructed ZLayer that will build the target types. This, of course, may
     * fail with one or more GraphErrors.
     */
    val layerTreeEither: Either[::[GraphError], LayerTree[ZExpr]] = {
      val nodes = providedLayerNodes ++ remainderNodes ++ sideEffectNodes
      val graph = Graph(nodes, (l, r) => r.conforms(l))

      for {
        original    <- graph.buildComplete(target)
        sideEffects <- graph.buildNodes(sideEffectNodes)
      } yield sideEffects ++ original
    }

    layerTreeEither match {
      case Left(buildErrors) => Left(graphToConstructionErrors(buildErrors))
      case Right(tree)       => warnUnused(tree)
    }
  }

  /**
   * Given a LayerTree, this will warn about all provided layers that are not
   * used. This will also warn about any specified remainders that aren't
   * actually required, in the case of provideSome/provideCustom.
   */
  private def warnUnused(tree: LayerTree[ZExpr]): Either[ConstructionWarning, Unit] = {
    val usedLayers = tree.toSet

    val unusedUserLayers        = providedLayerNodes.map(_.value).toSet -- usedLayers -- remainderNodes.map(_.value)
    val unusedUserLayersWarning = Option.when(unusedUserLayers.nonEmpty)(UnusedLayersWarning(unusedUserLayers))

    val emptyRemainderWarning =
      method match {
        case ProvideMethod.ProvideSome | ProvideMethod.ProvideSomeShared =>
          Option.when(remainder.isEmpty)(ProvideSomeAnyEnvWarning)
        case _ => None
      }

    val unusedRemainderLayers = remainderNodes.filterNot(node => usedLayers(node.value))
    val unusedRemainderLayersWarning =
      method match {
        case ProvideMethod.Provide => None
        case ProvideMethod.ProvideSome | ProvideMethod.ProvideSomeShared =>
          Option.when(!method.isProvideSomeShared && unusedRemainderLayers.nonEmpty) {
            UnusedProvideSomeLayersWarning(unusedRemainderLayers.map(node => node.outputs.head))
          }
        case ProvideMethod.ProvideCustom =>
          Option.when(unusedRemainderLayers.length == remainderNodes.length)(SuperfluousProvideCustomWarning)
      }

    val warnings = List(unusedUserLayersWarning, unusedRemainderLayersWarning, emptyRemainderWarning).flatten

    warnings match {
      case nonEmpty @ ::(_, _) => Left(Warnings(nonEmpty))
      case Nil                 => Right(())
    }
  }

  private def graphToConstructionErrors(errors: ::[GraphError]): ConstructionError = {
    val allErrors = sortErrors(errors)

    val topLevelErrors = allErrors.collect {
      case GraphError.MissingTopLevelDependency(layer) => layer
    }.distinct

    val transitive = allErrors.collect {
      case GraphError.MissingTransitiveDependencies(layer, deps) => layer.value -> deps
    }.groupBy(_._1).map { case (key, value) => key -> value.flatMap(_._2).distinct }

    val circularErrors = allErrors.collect {
      case GraphError.CircularDependency(layer, dep, _) => layer.value -> dep.value
    }

    if (circularErrors.nonEmpty)
      CircularityError(circularErrors)
    else
      MissingLayersError(topLevelErrors, transitive)
  }

  /**
   * Return only the first level of circular dependencies, as these will be the
   * most relevant.
   */
  private def sortErrors(errors: ::[GraphError]): Seq[GraphError] = {
    val (circularDependencyErrors, otherErrors) =
      errors.distinct.partitionMap {
        case circularDependency: GraphError.CircularDependency => Left(circularDependency)
        case other                                             => Right(other)
      }
    val sorted                = circularDependencyErrors.sortBy(_.depth)
    val initialCircularErrors = sorted.takeWhile(_.depth == sorted.headOption.map(_.depth).getOrElse(0))

    val (transitiveDepErrors, remainingErrors) = otherErrors.partitionMap {
      case es: GraphError.MissingTransitiveDependencies => Left(es)
      case other                                        => Right(other)
    }

    val groupedTransitiveErrors = transitiveDepErrors.groupBy(_.node).map {
      case (node, errors) =>
        val layer = errors.flatMap(_.dependency)
        GraphError.MissingTransitiveDependencies(node, layer)
    }

    initialCircularErrors ++ groupedTransitiveErrors ++ remainingErrors
  }

  private def layerToNode(expr: ZExpr): Option[Node] =
    expr.value match {
      case Typeable(`ZLayer[RIn, E, ROut]`(in, _, out)) =>
        val inputs = split(in).toList.filterNot(_.equiv(anyStdType)).map(new ZType(_))
        val outputs = split(out).toList match {
          case debug :: Nil if debugLayerType.exists(debug.conforms) => None
          case outs                                                  => Some(outs.map(new ZType(_)))
        }

        outputs.map(Node(inputs, _, expr))
      case _ => None
    }

  private def typeToNode(tpe: ZType): Node =
    Node(
      Nil,
      List(tpe),
      new ZExpr(createExpressionFromText(s"_root_.zio.ZLayer.environment[$tpe]", scalaFeatures))
    )

}

object LayerBuilder {

  // dirty hack to make it work
  // ScType and ScExpression don't have equals / hashCode methods, which makes it difficult to use it with the algorithm
  // besides, original ZIO algorithm uses string comparison too
  final class ZType(val value: ScType)(implicit context: TypePresentationContext) {
    def conforms(that: ZType): Boolean = this.value.conforms(that.value)

    override def equals(other: Any): Boolean = other match {
      case that: ZType => this.asStr == that.asStr
      case _           => false
    }
    override def hashCode(): Int  = asStr.hashCode
    override def toString: String = asStr

    private lazy val widened = value.widen
    private lazy val asStr   = resolveAliases(widened).getOrElse(widened).presentableText
  }

  final class ZExpr(val value: ScExpression)(implicit context: TypePresentationContext) {
    override def equals(other: Any): Boolean = other match {
      case that: ZExpr => this.asStr == that.asStr
      case _           => false
    }
    override def hashCode(): Int  = asStr.hashCode
    override def toString: String = asStr

    private lazy val asStr = value.getText
  }

  sealed trait ProvideMethod {
    def isProvideSomeShared: Boolean = this == ProvideMethod.ProvideSomeShared
    def isProvideSome: Boolean       = this == ProvideMethod.ProvideSome || this == ProvideMethod.ProvideSomeShared
  }

  object ProvideMethod {
    case object Provide           extends ProvideMethod
    case object ProvideSome       extends ProvideMethod
    case object ProvideSomeShared extends ProvideMethod
    case object ProvideCustom     extends ProvideMethod
  }

  sealed trait ConstructionIssue

  sealed trait ConstructionError                                            extends ConstructionIssue
  final case class DuplicateLayersError(duplicates: Map[ZType, Seq[ZExpr]]) extends ConstructionError
  final case class MissingLayersError(topLevel: Seq[ZType], transitives: Map[ZExpr, Seq[ZType]])
      extends ConstructionError
  final case class CircularityError(circular: Seq[(ZExpr, ZExpr)]) extends ConstructionError

  sealed trait ConstructionWarning                                    extends ConstructionIssue
  final case class UnusedLayersWarning(layers: Set[ZExpr])            extends ConstructionWarning
  final case class UnusedProvideSomeLayersWarning(layers: Seq[ZType]) extends ConstructionWarning
  final case class Warnings(w: ::[ConstructionWarning])               extends ConstructionWarning
  case object SuperfluousProvideCustomWarning                         extends ConstructionWarning
  case object ProvideSomeAnyEnvWarning                                extends ConstructionWarning

}

sealed abstract class LayerTree[+A] { self =>

  def >>>[A1 >: A](that: LayerTree[A1]): LayerTree[A1] =
    if (self eq Empty) that else if (that eq Empty) self else ComposeV(self, that)

  def ++[A1 >: A](that: LayerTree[A1]): LayerTree[A1] =
    if (self eq Empty) that else if (that eq Empty) self else ComposeH(self, that)

  def fold[B](z: B, value: A => B, composeH: (B, B) => B, composeV: (B, B) => B): B = self match {
    case Empty         => z
    case Value(value0) => value(value0)
    case ComposeH(left, right) =>
      composeH(left.fold(z, value, composeH, composeV), right.fold(z, value, composeH, composeV))
    case ComposeV(left, right) =>
      composeV(left.fold(z, value, composeH, composeV), right.fold(z, value, composeH, composeV))
  }

  def toSet[A1 >: A]: Set[A1] = fold[Set[A1]](Set.empty[A1], Set(_), _ ++ _, _ ++ _)

}

object LayerTree {
  def succeed[A](value: A): LayerTree[A] = Value(value)

  case object Empty                                                      extends LayerTree[Nothing]
  final case class Value[+A](value: A)                                   extends LayerTree[A]
  final case class ComposeH[+A](left: LayerTree[A], right: LayerTree[A]) extends LayerTree[A]
  final case class ComposeV[+A](left: LayerTree[A], right: LayerTree[A]) extends LayerTree[A]

  implicit final class LayerComposeIterableOps[A](private val self: Iterable[LayerTree[A]]) extends AnyVal {
    def combineHorizontally: LayerTree[A] = self.foldLeft[LayerTree[A]](Empty)(_ ++ _)
  }
}

final case class Node(inputs: List[ZType], outputs: List[ZType], value: ZExpr)

sealed trait GraphError

object GraphError {
  def missingTransitiveDependency(node: Node, dependency: ZType): GraphError =
    MissingTransitiveDependencies(node, Seq(dependency))

  case class MissingTransitiveDependencies(node: Node, dependency: Seq[ZType]) extends GraphError
  case class MissingTopLevelDependency(requirement: ZType)                     extends GraphError
  case class CircularDependency(node: Node, dependency: Node, depth: Int = 0)  extends GraphError
}

final case class Graph(nodes: List[Node], keyEquals: (ZType, ZType) => Boolean) {

  def buildComplete(outputs: List[ZType]): Either[::[GraphError], LayerTree[ZExpr]] =
    forEach(outputs) { output =>
      getNodeWithOutput(output, error = GraphError.MissingTopLevelDependency(output))
        .flatMap(node => buildNode(node, Set(node)))
    }
      .map(_.distinct.combineHorizontally)

  def buildNodes(nodes: List[Node]): Either[::[GraphError], LayerTree[ZExpr]] =
    forEach(nodes)(buildNode).map(_.combineHorizontally)

  private def buildNode(node: Node): Either[::[GraphError], LayerTree[ZExpr]] =
    forEach(node.inputs) { output =>
      getNodeWithOutput(output, error = GraphError.missingTransitiveDependency(node, output))
        .flatMap(node => buildNode(node, Set(node)))
    }
      .map(_.distinct.combineHorizontally)
      .map(_ >>> LayerTree.succeed(node.value))

  private def getNodeWithOutput(output: ZType, error: GraphError): Either[::[GraphError], Node] =
    nodes.find(_.outputs.exists(keyEquals(_, output))).toRight(::(error, Nil))

  private def buildNode(node: Node, seen: Set[Node]): Either[::[GraphError], LayerTree[ZExpr]] =
    forEach(node.inputs) { input =>
      for {
        out    <- getNodeWithOutput(input, error = GraphError.missingTransitiveDependency(node, input))
        _      <- assertNonCircularDependency(node, seen, out)
        result <- buildNode(out, seen + out)
      } yield result
    }.map {
      _.distinct.combineHorizontally >>> LayerTree.succeed(node.value)
    }

  private def assertNonCircularDependency(node: Node, seen: Set[Node], dependency: Node): Either[::[GraphError], Unit] =
    if (seen(dependency))
      Left(::(GraphError.CircularDependency(node, dependency, seen.size), Nil))
    else
      Right(())

  private def forEach[B, C](
    list: List[B]
  )(f: B => Either[::[GraphError], C]): Either[::[GraphError], List[C]] =
    list.foldRight[Either[::[GraphError], List[C]]](Right(List.empty)) { (a, b) =>
      (f(a), b) match {
        case (Left(::(e, es)), Left(e1s)) => Left(::(e, es ++ e1s))
        case (Left(es), _)                => Left(es)
        case (_, Left(es))                => Left(es)
        case (Right(a), Right(b))         => Right(a +: b)
      }
    }
}

object ErrorRendering {

  def missingLayersError(
    toplevel: Seq[ZType],
    transitive: Map[ZExpr, Seq[ZType]],
    isUsingProvideSome: Boolean = true
  ): String = {
    var index = 0
    def next = { index += 1; index }
    val indices          = collection.mutable.Map.empty[ZType, String]
    def id(layer: ZType) = indices.getOrElseUpdate(layer, s"$next.")

    val topLevelString = toplevel.map(layer => s"${id(layer)} $layer").mkString(lineSeparator).indent(indent)

    val transitiveStrings = transitive.map {
      case (layer, deps) =>
        val depsString = deps.map(dep => s"${id(dep)} $dep").mkString(lineSeparator)
        s"Required by $layer\n" + depsString
    }.mkString(lineSeparator).indent(indent)

    val header = s"Please provide layers for the following ${pluralizeTypes(index)}:"

    List(header, topLevelString, transitiveStrings).mkString(lineSeparator)
  }

  def ambigousLayersError(tpe: ZType, providedBy: Seq[ZExpr]): String =
    s"""Ambiguous layers! $tpe is provided by:
       |${providedBy.mkString(lineSeparator)}""".stripMargin

  def circularityError(a: ZExpr, b: ZExpr): String =
    s"""|Circular Dependency Detected
        |A layer simultaneously requires and is required by another:
        |  "◉" $b

        |  "╰─◉" $a

        |    "╰─ ◉" $b""".stripMargin

  def unusedProvideSomeLayersWarning(layers: Seq[ZType]): String =
    s"""You have provided more arguments to provideSome than is required.
       |You may remove the following ${pluralizeTypes(layers.size)}:
       |${layers.mkString(", ")}""".stripMargin

  val unusedLayersWarning =
    "You have provided more than is required. You may remove this layer."
  val superfluousProvideCustomWarning =
    "You are using provideCustom unnecessarily. None of the default services are required. Simply use provide instead."
  val provideSomeAnyEnvWarning =
    "You are using provideSome unnecessarily. The layer is fully satisfied. Simply use provide instead."

  private val indent = 1

  private def pluralizeTypes(n: Int): String =
    if (n == 1) "type" else s"$n types"

}
