package zio.intellij.inspections

import com.intellij.codeInspection.{ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.codeInspection.collections.OperationOnCollectionInspectionBase.SimplifiableExpression
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.utils.{ModuleSyntax, ZioVersion}

import javax.swing.JComponent

abstract class ZInspection(simplifiers: SimplificationType*) extends OperationOnCollectionInspection {
  final override def getLikeCollectionClasses: Seq[String] = List("zio.ZIO")

  final override def createOptionsPanel(): JComponent = null // god help me

  final override def possibleSimplificationTypes: Seq[SimplificationType] = simplifiers

  protected def isAvailable(zioVersion: ZioVersion): Boolean = zioVersion >= ZioVersion.ZIO.`1.0.0`

  private def isInspectionAvailable(inspection: ZInspection, element: PsiElement): Boolean =
    Option(ScalaPsiUtil.getModule(element))
      .flatMap(_.zioVersion)
      .exists(zioVersion => inspection.isAvailable(zioVersion))

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case SimplifiableExpression(expr) if isInspectionAvailable(this, expr) =>
      simplifications(expr).foreach {
        case s @ Simplification(toReplace, _, hint, rangeInParent) =>
          val quickFix = OperationOnCollectionQuickFix(s)
          holder.registerProblem(
            toReplace.getElement,
            hint,
            ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
            rangeInParent,
            quickFix
          )
      }
    case _ =>
  }

  private def simplifications(expr: ScExpression): Seq[Simplification] = {
    def simplificationTypes = for {
      (st, idx) <- possibleSimplificationTypes.zipWithIndex
      if simplificationTypesEnabled(idx)
    } yield st

    simplificationTypes.flatMap(st => st.getSimplifications(expr) ++ st.getSimplification(expr))
  }

}
