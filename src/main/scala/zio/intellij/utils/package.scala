package zio.intellij

import com.intellij.openapi.module.Module
import com.intellij.openapi.project.{Project, ProjectUtil}
import com.intellij.openapi.roots.OrderRootType
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.{JavaPsiFacade, PsiElement}
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.annotator.usageTracker.ScalaRefCountHolder
import org.jetbrains.plugins.scala.extensions.PsiClassExt
import org.jetbrains.plugins.scala.lang.psi.api.base.ScFieldId
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScNamedElement, ScTypedDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.project.{LibraryExt, ModuleExt, ProjectExt, ScalaLanguageLevel}
import org.jetbrains.sbt.SbtUtil
import org.jetbrains.sbt.SbtUtil.getDefaultLauncher
import org.jetbrains.sbt.project.SbtExternalSystemManager

import java.io.File
import scala.annotation.tailrec

package object utils {

  implicit class ProjectSyntax(private val project: Project) extends AnyVal {
    def versions: List[(Version, ScalaVersion)] = {
      val sourceModules = project.modulesWithScala.filter(_.isSourceModule).toList

      sourceModules.flatMap { m =>
        // First ZIO Test runner release: RC18-2
        // Do not try to download test runner for ZIO versions without runner release
        val zioVersion = m.zioVersion.filter(_ >= Version.ZIO.`RC18-2`)
        zioVersion zip m.scalaVersion
      }.distinct
    }

    def sbtVersion = {
      val workingDirPath =
        Option(ProjectUtil.guessProjectDir(project))
          .getOrElse(throw new IllegalStateException(s"no project directory found for project ${project.getName}"))
          .getCanonicalPath
      val workingDir = new File(workingDirPath)

      val sbtSettings = SbtExternalSystemManager.executionSettingsFor(project, workingDirPath)
      val launcher    = sbtSettings.customLauncher.getOrElse(getDefaultLauncher)

      SbtUtil.detectSbtVersion(workingDir, launcher)
    }
  }

  implicit class StringBuilderSyntax(private val builder: StringBuilder) extends AnyVal {
    def appendLine: StringBuilder              = builder.append(System.lineSeparator())
    def appendLine(str: String): StringBuilder = builder.append(str).appendLine
  }

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

  def createTypeElement(text: String, context: PsiElement, child: PsiElement = null): Option[ScTypeElement] =
    ScalaPsiElementFactory.safe(_.createTypeElementFromText(text, context, child))

  def createTypeElement(tpe: ScType, context: PsiElement)(implicit
    tpc: TypePresentationContext
  ): Option[ScTypeElement] =
    createTypeElement(tpe.codeText, context)

  def createExpression(text: String, context: PsiElement): Option[ScExpression] =
    ScalaPsiElementFactory.safe(_.createExpressionFromText(text, context))

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

  implicit class ModuleSyntax(private val module: Module) extends AnyVal {
    def findLibrary(p: String => Boolean): Option[Version] =
      (for {
        library <- module.libraries
        url     <- library.getUrls(OrderRootType.CLASSES)
        if p(url)
        trimmedUrl  = utils.trimAfterSuffix(url, ".jar")
        versionStr <- LibraryExt.runtimeVersion(trimmedUrl)
        version    <- Version.parse(versionStr)
      } yield version).headOption

    def zioVersion: Option[Version] = findLibrary(lib => lib.contains("/dev/zio/zio_") || lib.contains("/dev.zio/zio_"))

    def hasZio = zioVersion.isDefined

    def scalaVersion =
      for {
        scalaSdk     <- module.scalaSdk
        compiler     <- scalaSdk.compilerVersion
        scalaVersion <- ScalaVersion.fromString(compiler)
      } yield scalaVersion
  }

  /**
   * The release version of Scala 3 changes the classifier used to resolve dependencies,
   * whereas in pre-3.0.0 it used the Scala 2 scheme, e.g. artifact_name:3.0.0-RC2,
   * the release version uses a single major version digit, i.e. artifact_name:3.
   *
   * More info: https://www.scala-lang.org/blog/2021/04/08/scala-3-in-sbt.html
   */
  implicit class ScalaVersionHack(private val version: ScalaVersion) extends AnyVal {
    def versionStr = version.languageLevel match {
      case ScalaLanguageLevel.Scala_3_0 if version.isPrerelease => version.minor
      case ScalaLanguageLevel.Scala_3_0                         => "3"
      case _                                                    => version.major
    }

    def isPrerelease = version < Version.scala3Version
  }

  implicit class TraverseAtHome[A](private val list: List[A]) extends AnyVal {
    def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
      oa.flatMap(a => ob.map(b => f(a, b)))

    def traverse[B](f: A => Option[B]): Option[List[B]] =
      list.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
  }

  implicit final class ListSyntax[A](private val list: List[A]) extends AnyVal {

    // Similar to .minBy, but returns all minimal elements from original list
    def minsBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = {
      @tailrec
      def loop(currMin: B, list: List[A], acc: List[A]): List[A] =
        list match {
          case Nil => acc.reverse
          case head :: tail =>
            val currRes = f(head)

            if (ord.lt(currRes, currMin)) loop(currRes, tail, List(head))
            else if (ord.equiv(currRes, currMin)) loop(currMin, tail, head +: acc)
            else loop(currMin, tail, acc)
        }

      list match {
        case Nil          => Nil
        case head :: tail => loop(f(head), tail, List(head))
      }
    }
  }

}
