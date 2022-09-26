package zio.intellij.inspections

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple

import javax.swing.JComponent
import org.jetbrains.plugins.scala.codeInspection.collections.OperationOnCollectionInspectionBase.SimplifiableExpression
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.utils.{ModuleSyntax, Version}

abstract class ZInspection(simplifiers: SimplificationType*) extends OperationOnCollectionInspection {
  final override def getLikeCollectionClasses: Seq[String] = List("zio.ZIO")

  final override def createOptionsPanel(): JComponent = null // god help me

  final override def possibleSimplificationTypes: Seq[SimplificationType] = simplifiers

  protected def isAvailable(zioVersion: Version): Boolean = zioVersion >= Version.ZIO.`1.0.0`

  private def isInspectionAvailable(inspection: ZInspection, element: PsiElement): Boolean =
    Option(ScalaPsiUtil.getModule(element))
      .flatMap(_.zioVersion)
      .exists(zioVersion => inspection.isAvailable(zioVersion))

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case SimplifiableExpression(expr) if isInspectionAvailable(this, expr) =>
      simplifications(expr).foreach {
        case s @ Simplification(toReplace, _, hint, rangeInParent) =>
          val quickFix = OperationOnCollectionQuickFix(s)
          holder.registerProblem(toReplace.getElement, hint, highlightType, rangeInParent, quickFix)
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
