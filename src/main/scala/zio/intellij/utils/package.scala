package zio.intellij

import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.{JavaPsiFacade, PsiElement}
import org.jetbrains.plugins.scala.annotator.usageTracker.ScalaRefCountHolder
import org.jetbrains.plugins.scala.extensions.PsiClassExt
import org.jetbrains.plugins.scala.lang.psi.api.base.ScFieldId
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScNamedElement, ScTypedDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

package object utils {

  // taken from ScalaUnusedSymbolInspection
  def isElementUsed(element: ScNamedElement, isOnTheFly: Boolean): Boolean =
    if (isOnTheFly) {
      //we can trust RefCounter because references are counted during highlighting
      val refCounter = ScalaRefCountHolder(element)
      var used       = false

      val success = refCounter.retrieveUnusedReferencesInfo { () =>
        used |= refCounter.isValueReadUsed(element) || refCounter.isValueWriteUsed(element)
      }

      !success || used //want to return true if it was a failure
    } else
      //need to look for references because file is not highlighted
      ReferencesSearch.search(element, element.getUseScope).findFirst() != null

  // CompositeOrdering is taken from https://stackoverflow.com/a/14696410
  final class CompositeOrdering[T](val ord1: Ordering[T], val ord2: Ordering[T]) extends Ordering[T] {

    def compare(x: T, y: T): Int = {
      val comp = ord1.compare(x, y)
      if (comp != 0) comp else ord2.compare(x, y)
    }
  }

  object CompositeOrdering {
    def apply[T](orderings: Ordering[T]*): Ordering[T] = orderings.reduceLeft(_.orElse(_))
  }

  implicit final class OrderingOps[T](private val ord: Ordering[T]) extends AnyVal {
    def orElse(ord2: Ordering[T]) = new CompositeOrdering[T](ord, ord2)
  }

  def trimAfterSuffix(str: String, suffix: String): String = {
    val idx = str.lastIndexOf(suffix)
    if (idx < 0) str
    else str.substring(0, idx + suffix.length)
  }

  def findTypeDefByName(project: Project, qualifiedName: String): Option[ScTypeDefinition] =
    JavaPsiFacade.getInstance(project).findClass(qualifiedName, GlobalSearchScope.projectScope(project)) match {
      case typeDef: ScTypeDefinition => Some(typeDef)
      case _                         => None
    }

  def createType(text: String, context: PsiElement, child: PsiElement = null): Option[ScType] =
    ScalaPsiElementFactory.createTypeFromText(text, context, child)

  @annotation.tailrec
  def resolveAliases(tpe: ScType): Option[ScType] =
    if (!tpe.isAliasType) Some(tpe)
    else
      tpe.aliasType match {
        case Some(AliasType(_: ScTypeAliasDefinition, Right(l), Right(h))) if l == h =>
          resolveAliases(l)
        case Some(AliasType(typeDef: ScTypeAliasDefinition, _, _)) =>
          typeDef.aliasedType match {
            case Right(aliasedType) => resolveAliases(aliasedType)
            case Left(_)            => None
          }
        case _ => None
      }

  def extractTypeArguments(tpe: ScType): Option[Seq[ScType]] =
    tpe match {
      case parameterizedType: ScParameterizedType => Some(parameterizedType.typeArguments)
      case _                                      => None
    }

  def fqnIfIsOfClassFrom(tpe: ScType, patterns: Array[String]): Option[String] =
    tpe.tryExtractDesignatorSingleton.extractClass
      .flatMap(Option(_))
      .flatMap(c => Option(c.qualifiedName))
      .find(ScalaNamesUtil.nameFitToPatterns(_, patterns, strict = false))

  object Field {

    def unapply(ts: TermSignature): Option[ScTypedDefinition] =
      Some(ts.namedElement).collect {
        case fid: ScFieldId          => fid
        case ref: ScReferencePattern => ref
      }
  }

  object Method {

    def unapply(ts: TermSignature): Option[ScFunction] =
      ts match {
        case PhysicalMethodSignature(method: ScFunctionDeclaration, _) => Some(method)
        case PhysicalMethodSignature(method: ScFunctionDefinition, _)  => Some(method)
        case _                                                         => None
      }
  }

}
