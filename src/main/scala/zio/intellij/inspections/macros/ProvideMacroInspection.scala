package zio.intellij.inspections.macros

import com.intellij.codeInspection._
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenericCall, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.{
  createExpressionFromText,
  createTypeElementFromText
}
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{api, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.project.{ProjectContext, ProjectPsiElementExt, ScalaFeatures}
import zio.intellij.inspections.macros.LayerBuilder._
import zio.intellij.inspections.macros.LayerTree.{ComposeH, ComposeV, Empty, Value}
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._

import java.lang.System.lineSeparator
import scala.util.chaining.scalaUtilChainingOps

class ProvideMacroInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = element => {
    val module = element.module
    if (element.isInScala3File) () // inspection causes lots of false positives in Scala 3. Disable until better times
    else if (module.exists(_.isZio1)) visitZIO1ProvideMethods(holder)(element)
    else if (module.exists(_.isZio2)) visitZIO2ProvideMethods(holder)(element)
  }

  private def visitZIO1ProvideMethods(holder: ProblemsHolder)(element: PsiElement): Unit = element match {
    case expr @ `.inject`(base, layers @ _*) =>
      tryBuildProvideZIO1(base, layers).fold(visitIssue(holder, expr), identity)
    case fullyApplied @ ScMethodCall(partiallyApplied @ `.injectSome`(base, _ @_*), layers) =>
      visitProvideSomeZIO1(partiallyApplied, base, layers).fold(visitIssue(holder, fullyApplied), identity)
    case expr @ `.injectShared`(base, layers @ _*) =>
      tryBuildProvideZIO1(base, layers).fold(visitIssue(holder, expr), identity)
    case fullyApplied @ ScMethodCall(partiallyApplied @ `.injectSomeShared`(base, _ @_*), layers) =>
      visitProvideSomeSharedZIO1(partiallyApplied, base, layers).fold(visitIssue(holder, fullyApplied), identity)
    case _ =>
  }

  private def visitZIO2ProvideMethods(holder: ProblemsHolder)(element: PsiElement): Unit = element match {
    case expr @ `.provide`(base, layers @ _*) =>
      tryBuildProvideZIO2(base, layers).fold(visitIssue(holder, expr), identity)
    case fullyApplied @ ScMethodCall(partiallyApplied @ `.provideSome`(base, _ @_*), layers) =>
      visitProvideSomeZIO2(partiallyApplied, base, layers).fold(visitIssue(holder, fullyApplied), identity)
    case expr @ `.provideShared`(base, layers @ _*) =>
      tryBuildProvideZIO2(base, layers).fold(visitIssue(holder, expr), identity)
    case fullyApplied @ ScMethodCall(partiallyApplied @ `.provideSomeShared`(base, _ @_*), layers) =>
      visitProvideSomeSharedZIO2(partiallyApplied, base, layers).fold(visitIssue(holder, fullyApplied), identity)
    case _ =>
  }

  private def tryBuildProvideZIO1(base: ScExpression, layers: Seq[ScExpression]): Either[ConstructionIssue, Unit] =
    base match {
      case Typeable(`ZIO[R, E, A]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO1(base)(
            target = split(r),
            remainder = Nil,
            providedLayers = layers,
            method = ProvideMethod.Provide
          )
      case Typeable(`zio1.Spec[R, E, T]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO1(base)(
            target = split(r),
            remainder = Nil,
            providedLayers = layers,
            method = ProvideMethod.Provide
          )
      case _ =>
        Right(())
    }

  private def tryBuildProvideZIO2(base: ScExpression, layers: Seq[ScExpression]): Either[ConstructionIssue, Unit] =
    base match {
      case Typeable(`ZIO[R, E, A]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO2(base)(
            target = split(r),
            remainder = Nil,
            providedLayers = layers,
            method = ProvideMethod.Provide
          )
      case Typeable(`zio2.Spec[R, E]`(r, _)) =>
        LayerBuilder
          .tryBuildZIO2(base)(
            target = split(r),
            remainder = Nil,
            providedLayers = layers,
            method = ProvideMethod.Provide
          )
      case _ =>
        Right(())
    }

  private def visitProvideSomeZIO1(
    expr: ScExpression,       // effectLike.injectSome[Foo]
    base: ScExpression,       // effectLike
    layers: Seq[ScExpression] // layer1, layer2
  ): Either[ConstructionIssue, Unit] =
    base match {
      case Typeable(`ZIO[R, E, A]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO1(expr)(
            target = split(r),
            remainder = methodTypeArgs(expr).flatMap(split),
            providedLayers = layers,
            method = ProvideMethod.ProvideSome
          )
      case Typeable(`zio1.Spec[R, E, T]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO1(expr)(
            target = split(r),
            remainder = methodTypeArgs(expr).flatMap(split),
            providedLayers = layers,
            method = ProvideMethod.ProvideSome
          )
      case _ =>
        Right(())
    }

  private def visitProvideSomeZIO2(
    expr: ScExpression,       // effectLike.provideSome[Foo]
    base: ScExpression,       // effectLike
    layers: Seq[ScExpression] // layer1, layer2
  ): Either[ConstructionIssue, Unit] =
    base match {
      case Typeable(`ZIO[R, E, A]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO2(expr)(
            target = split(r),
            remainder = methodTypeArgs(expr).flatMap(split),
            providedLayers = layers,
            method = ProvideMethod.ProvideSome
          )
      case Typeable(`zio2.Spec[R, E]`(r, _)) =>
        LayerBuilder
          .tryBuildZIO2(expr)(
            target = split(r),
            remainder = methodTypeArgs(expr).flatMap(split),
            providedLayers = layers,
            method = ProvideMethod.ProvideSome
          )
      case _ =>
        Right(())
    }

  private def visitProvideSomeSharedZIO1(
    expr: ScExpression,       // effectLike.injectSome[Foo]
    base: ScExpression,       // effectLike
    layers: Seq[ScExpression] // layer1, layer2
  ): Either[ConstructionIssue, Unit] =
    base match {
      case Typeable(`zio1.Spec[R, E, T]`(r, _, _)) =>
        LayerBuilder
          .tryBuildZIO1(expr)(
            target = split(r),
            remainder = methodTypeArgs(expr).flatMap(split),
            providedLayers = layers,
            method = ProvideMethod.ProvideSomeShared
          )
      case _ =>
        Right(())
    }

  private def visitProvideSomeSharedZIO2(
    expr: ScExpression,       // effectLike.provideSome[Foo]
    base: ScExpression,       // effectLike
    layers: Seq[ScExpression] // layer1, layer2
  ): Either[ConstructionIssue, Unit] =
    base match {
      case Typeable(`zio2.Spec[R, E]`(r, _)) =>
        LayerBuilder
          .tryBuildZIO2(expr)(
            target = split(r),
            remainder = methodTypeArgs(expr).flatMap(split),
            providedLayers = layers,
            method = ProvideMethod.ProvideSomeShared
          )
      case _ =>
        Right(())
    }

  private def methodTypeArgs(expr: ScExpression): Seq[ScType] =
    expr match {
      case ScGenericCall(_, typeArgs) => typeArgs.flatMap(_.`type`().toOption)
      case _                          => Seq.empty
    }

  private def visitIssue(holder: ProblemsHolder, expr: ScExpression)(issue: ConstructionIssue): Unit =
    issue match {
      case error: ConstructionError     => visitError(holder, expr)(error)
      case warning: ConstructionWarning => visitWarning(holder, expr)(warning)
    }

  private val errorHighlight   = ProblemHighlightType.GENERIC_ERROR
  private val warningHighlight = ProblemHighlightType.WEAK_WARNING

  import ErrorRendering._

  private def visitError(holder: ProblemsHolder, expr: ScExpression)(error: ConstructionError): Unit =
    error match {
      case MissingLayersError(topLevel, transitives, isProvideSome) =>
        holder.registerProblem(expr, missingLayersError(topLevel, transitives, isProvideSome), errorHighlight)
      case DuplicateLayersError(duplicates) =>
        duplicates.foreach {
          case (tpe, exprs) =>
            exprs.foreach { expr =>
              holder.registerProblem(expr.value, ambigousLayersError(tpe, exprs), errorHighlight)
            }
        }
      case CircularityError(circular) =>
        circular.foreach {
          case (a, b) =>
            holder.registerProblem(expr, circularityError(a, b), errorHighlight)
        }
      case NonHasTypesError(types) =>
        holder.registerProblem(expr, nonHasTypeError(types), errorHighlight)
    }

  private def visitWarning(holder: ProblemsHolder, expr: ScExpression)(warning: ConstructionWarning): Unit =
    warning match {
      case UnusedLayersWarning(layers) =>
        layers.foreach(layer => holder.registerProblem(layer.value, unusedLayersWarning, warningHighlight))
      case UnusedProvideSomeLayersWarning(layers) =>
        holder.registerProblem(expr, unusedProvideSomeLayersWarning(layers), warningHighlight)
      case SuperfluousProvideCustomWarning =>
        holder.registerProblem(expr, superfluousProvideCustomWarning, warningHighlight)
      case ProvideSomeAnyEnvWarning =>
        holder.registerProblem(expr, provideSomeAnyEnvWarning, warningHighlight)
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
 * @param providedLayerNodes
 *   A list of layers that have been provided by the user to build services
 * @param sideEffectNodes
 *   A list of layers that have been provided by the users that might contain side effects, might also be used to build services.
 *   These nodes are guaranteed to not cause "unused layer" warning.
 * @param method
 *   The sort of method that is being called: `provide`, `provideSome`, or
 *   `provideCustom`. This is used to provide improved warnings.
 *  @param typeToLayer
 *  to construct a `ZLayer` that summons required type
 */
final case class LayerBuilder(
  target0: List[ZType],
  remainder: List[ZType],
  providedLayerNodes: List[Node],
  sideEffectNodes: List[Node],
  method: ProvideMethod,
  typeToLayer: ZType => String
)(implicit pContext: ProjectContext, scalaFeatures: ScalaFeatures) {

  def tryBuild: Either[ConstructionIssue, Unit] = assertNoAmbiguity.flatMap(_ => tryBuildInternal)

  private val target =
    if (method.isProvideSomeShared) target0.filterNot(t => remainder.exists(_.isSubtypeOf(t)))
    else target0

  private val remainderNodes = remainder.map(typeToNode).distinct

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
      val graph = Graph(nodes, _.isSubtypeOf(_))

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

    val emptyRemainderWarning = Option.when(method.isProvideSome && remainder.isEmpty)(ProvideSomeAnyEnvWarning)

    val unusedRemainderLayers = remainderNodes.filterNot(node => usedLayers(node.value))
    val unusedRemainderLayersWarning =
      method match {
        case ProvideMethod.Provide | ProvideMethod.ProvideSomeShared => None
        case ProvideMethod.ProvideSome =>
          Option.when(unusedRemainderLayers.nonEmpty) {
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
      MissingLayersError(topLevelErrors, transitive, method.isProvideSome)
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

  private def typeToNode(tpe: ZType): Node =
    Node(
      inputs = Nil,
      outputs = List(tpe),
      value = new ZExpr(createExpressionFromText(typeToLayer(tpe), scalaFeatures))
    )

}

object LayerBuilder {

  // version specific: making sure ScType <: Has[_]
  def tryBuildZIO1(expr: ScExpression)(
    target: Seq[ScType],
    remainder: Seq[ScType],
    providedLayers: Seq[ScExpression],
    method: ProvideMethod
  ): Either[ConstructionIssue, Unit] = {
    implicit val tpContext: TypePresentationContext = expr
    implicit val pContext: ProjectContext           = expr
    implicit val scalaFeatures: ScalaFeatures       = expr

    val hasDesignator = createType("_root_.zio.Has", expr)

    def isHasType(tpe: ZType): Boolean =
      tpe.value match {
        case ParameterizedType(designator, _) => hasDesignator.exists(_.equiv(designator))
        case _                                => false
      }

    // we are not expecting to see non-Has types often in ZIO1 so it's dirty but efficient enough
    val nonHasTypes = collection.mutable.ListBuffer.empty[ZType]

    def toZType(tpe: ScType): Option[ZType] =
      ZType(tpe).tap(_.foreach(tpe => if (!isHasType(tpe)) nonHasTypes += tpe))

    def layerToNode(expr: ScExpression): Option[Node] =
      expr match {
        case Typeable(`ZLayer[RIn, E, ROut]`(in, _, out)) =>
          val inputs  = split(in).toList.flatMap(toZType)
          val outputs = split(out).toList.flatMap(toZType)
          Some(Node(inputs, outputs, new ZExpr(expr)))
        case _ => None
      }

    // do NOT inline
    // toZType is stateful
    val target0             = target.toList.flatMap(toZType)
    val remainder0          = remainder.toList.flatMap(toZType)
    val providedLayerNodes0 = providedLayers.toList.flatMap(layerToNode)

    if (containsNothingAsRequirement(target, remainder, providedLayerNodes0)) Right(())
    else if (nonHasTypes.nonEmpty)
      Left(NonHasTypesError(nonHasTypes.toSet))
    else
      LayerBuilder(
        target0 = target0,
        remainder = remainder0,
        providedLayerNodes = providedLayerNodes0,
        sideEffectNodes = Nil,
        method = method,
        typeToLayer = tpe => s"_root_.zio.ZLayer.requires[$tpe]"
      ).tryBuild
  }

  // version-specific: taking care of Debug and side-effect layers
  def tryBuildZIO2(expr: ScExpression)(
    target: Seq[ScType],
    remainder: Seq[ScType],
    providedLayers: Seq[ScExpression],
    method: ProvideMethod
  ): Either[ConstructionIssue, Unit] = {
    implicit val tpContext: TypePresentationContext = expr
    implicit val pContext: ProjectContext           = expr
    implicit val scalaFeatures: ScalaFeatures       = expr

    val debugLayer = createTypeElementFromText("_root_.zio.ZLayer.Debug", scalaFeatures).`type`().toOption

    def layerToNode(expr: ScExpression): Option[Node] =
      expr match {
        case Typeable(`ZLayer[RIn, E, ROut]`(in, _, out)) =>
          val inputs = split(in).toList.flatMap(ZType(_))
          val outputs = split(out).toList match {
            case debug :: Nil if debugLayer.exists(debug.conforms) => None
            case outs                                              => Some(outs.flatMap(ZType(_)))
          }

          outputs.map(Node(inputs, _, new ZExpr(expr)))
        case _ => None
      }

    val (sideEffectNodes, providedLayerNodes) =
      providedLayers.toList.flatMap(layerToNode).partition(_.outputs.exists(o => api.Unit.conforms(o.value)))

    if (containsNothingAsRequirement(target, remainder, providedLayerNodes)) Right(())
    else
      LayerBuilder(
        target0 = target.toList.flatMap(ZType(_)),
        remainder = remainder.toList.flatMap(ZType(_)),
        providedLayerNodes = providedLayerNodes,
        sideEffectNodes = sideEffectNodes,
        method = method,
        typeToLayer = tpe => s"_root_.zio.ZLayer.environment[$tpe]"
      ).tryBuild
  }

  // Sometimes IntelliJ fails to infer actual type and uses `Nothing` instead.
  // In that case, we should not report an error and just ignore it to not annoy people
  private def containsNothingAsRequirement(target: Seq[ScType], remainder: Seq[ScType], providedLayerNodes: List[Node])(
    implicit pc: ProjectContext
  ): Boolean =
    (target ++ remainder ++ providedLayerNodes.flatMap(_.inputs.map(_.value))).exists(_.equiv(api.Nothing))

  // dirty hack to make it work
  // ScType and ScExpression don't have equals / hashCode methods, which makes it difficult to use it with the algorithm
  // use canonicalText for comparison to avoid name collisions
  // also, ZType is guaranteed to have meaningful type (non-Any)
  final class ZType private (val value: ScType)(implicit context: TypePresentationContext) {
    def isSubtypeOf(that: ZType): Boolean = this.value.conforms(that.value)

    override def equals(other: Any): Boolean = other match {
      case that: ZType => this.canonicalText == that.canonicalText
      case _           => false
    }
    override def hashCode: Int    = canonicalText.hashCode
    override def toString: String = presentableText

    private lazy val widened   = value.widen
    private lazy val dealiased = resolveAliases(widened).getOrElse(widened)

    private lazy val canonicalText   = dealiased.canonicalText
    private lazy val presentableText = dealiased.presentableText
  }

  object ZType {

    def apply(value: ScType)(implicit pc: ProjectContext, tpc: TypePresentationContext): Option[ZType] =
      Option.unless(value.equiv(api.Any))(new ZType(value))

  }

  final class ZExpr(val value: ScExpression) {
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
  final case class MissingLayersError(topLevel: Seq[ZType], transitives: Map[ZExpr, Seq[ZType]], isProvideSome: Boolean)
      extends ConstructionError
  final case class CircularityError(circular: Seq[(ZExpr, ZExpr)]) extends ConstructionError
  final case class NonHasTypesError(types: Set[ZType])             extends ConstructionError

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
    isProvideSome: Boolean
  ): String = {
    var index = 0
    def next = { index += 1; index }
    val indices          = collection.mutable.Map.empty[ZType, String]
    def id(layer: ZType) = indices.getOrElseUpdate(layer, s"$next.")

    val topLevelString = Option.when(toplevel.nonEmpty) {
      toplevel.map(layer => s"${id(layer)} $layer").mkString(lineSeparator)
    }

    val transitiveStrings = Option.when(transitive.nonEmpty) {
      transitive.map {
        case (layer, deps) =>
          val depsString = deps.map(dep => s"${id(dep)} $dep").mkString(lineSeparator)
          s"Required by $layer\n" + depsString
      }.mkString(lineSeparator)
    }

    val header = Some(s"Please provide layers for the following ${pluralizeTypes(index)}:")

    val footer =
      Option.when(isProvideSome) {
        val allMissingTypes = toplevel ++ transitive.values.flatten
        s"""Alternatively, you may add them to the remainder type ascription:
           | .provideSome[${allMissingTypes.mkString(" & ")}]""".stripMargin
      }

    List(header, topLevelString, transitiveStrings, footer).flatten.mkString(lineSeparator)
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

  def nonHasTypeError(types: Set[ZType]): String =
    s"Contains non-Has types:$lineSeparator- ${types.mkString(s"$lineSeparator- ")}"

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

  private def pluralizeTypes(n: Int): String =
    if (n == 1) "type" else s"$n types"

}
