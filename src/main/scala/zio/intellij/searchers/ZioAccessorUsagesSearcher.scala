package zio.intellij.searchers

import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.search.searches.ReferencesSearch._
import com.intellij.psi.{PsiClass, PsiElement, PsiReference}
import com.intellij.util.{Processor, QueryExecutor}
import org.jetbrains.plugins.scala.extensions.{inReadAction, ContainingClass}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScFieldId
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTrait}
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, TermSignature}
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._

class ZioAccessorUsagesSearcher extends QueryExecutor[PsiReference, ReferencesSearch.SearchParameters] {

  private val zioTraitNames = Set("Service")

  object ContainingFieldClass {
    def unapply(element: ScFieldId): Option[PsiClass] =
      element.nameContext match {
        case ContainingClass(c) => Some(c)
        case _                  => None
      }
  }

  object ContainingObject {
    def unapply(element: PsiClass): Option[ScObject] =
      element match {
        case ContainingClass(obj: ScObject) => Some(obj)
        case _                              => None
      }
  }

  def execute(queryParameters: SearchParameters, consumer: Processor[_ >: PsiReference]): Boolean = {
    val element: PsiElement = queryParameters.getElementToSearch
    val accessorOpt = inReadAction {
      element match {
        case m: ScFunctionDeclaration => findMethodAccessor(m)
        case m: ScFunctionDefinition  => findMethodAccessor(m)
        case f: ScReferencePattern    => findFieldAccessor(f)
        case f: ScFieldId             => findFieldAccessor(f)
        case _                        => None
      }
    }
    accessorOpt match {
      case Some(accessor) =>
        val processor = new Processor[PsiReference] {
          override def process(ref: PsiReference): Boolean = inReadAction(consumer.process(ref))
        }
        ReferencesSearch
          .search(accessor, queryParameters.getEffectiveSearchScope, queryParameters.isIgnoreAccessScope)
          .forEach(processor)
      case _ => true
    }
  }

  private def findMethodAccessor(method: ScFunction): Option[PsiElement] = {
    val containingTrait = Some(method).collect {
      case ContainingClass(tr: ScTrait) => tr
    }
    val service = containingTrait.filter(tr => zioTraitNames(tr.name))
    val containingObject =
      service.collect {
        case ContainingObject(obj) => obj
      }

    containingObject.toSeq.flatMap(_.allMethods).find(isAccessorMethod(method, _)).map(_.namedElement)
  }

  private def findFieldAccessor(field: ScTypedDefinition): Option[PsiElement] = {
    val containingTrait = Some(field).collect {
      case ContainingClass(tr: ScTrait)      => tr
      case ContainingFieldClass(tr: ScTrait) => tr
    }
    val service = containingTrait.filter(tr => zioTraitNames(tr.name))
    val containingObject =
      service.collect {
        case ContainingObject(obj) => obj
      }

    containingObject.toSeq.flatMap(_.allVals).find(isAccessorField(field, _)).map(_.namedElement)
  }

  private def isAccessorMethod(method: ScFunction, candidate: PhysicalMethodSignature): Boolean = candidate match {
    case Method(acc) =>
      acc.name == method.name && fromZio(acc.returnType.getOrAny) &&
        areParamsSame(acc.parameters.toSeq, method.parameters.toSeq)
    case _ => false
  }

  private def isAccessorField(field: ScTypedDefinition, candidate: TermSignature): Boolean = candidate match {
    case Field(f) => f.name == field.name && fromZio(f.`type`.getOrAny)
    case _        => false
  }

  // no equals for ScParameter :(
  private def areParamsSame(seq1: Seq[ScParameter], seq2: Seq[ScParameter]): Boolean = {
    val isSameLength = seq1.length == seq2.length

    lazy val areSameElements =
      seq1.zip(seq2).forall {
        case (p1, p2) =>
          p1.name == p2.name && p1.`type` == p2.`type`
      }

    isSameLength && areSameElements
  }
}
