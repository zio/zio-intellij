package zio.intellij.inspections

import com.intellij.codeInspection.ProblemsHolder
import com.intellij.psi.PsiElement
import javax.swing.JComponent
import org.jetbrains.plugins.scala.codeInspection.collections.OperationOnCollectionInspectionBase.SimplifiableExpression
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.utils.{ModuleSyntax, Version}

abstract class ZInspection(simplifiers: SimplificationType*) extends OperationOnCollectionInspection {
  final override def getLikeCollectionClasses: Array[String] = Array("zio.ZIO")

  final override def createOptionsPanel(): JComponent = null // god help me

  final override def possibleSimplificationTypes: Array[SimplificationType] = simplifiers.toArray

  protected def isAvailable(zioVersion: Version): Boolean = zioVersion >= Version.ZIO.`1.0.0`

  private def isInspectionAvailable(inspection: ZInspection, element: PsiElement): Boolean = {
    Option(ScalaPsiUtil.getModule(element))
      .flatMap(_.zioVersion)
      .exists(zioVersion => inspection.isAvailable(zioVersion))
  }

  protected override def actionFor(implicit
    holder: ProblemsHolder,
    isOnTheFly: Boolean
  ): PartialFunction[PsiElement, Any] = {
    case SimplifiableExpression(expr) if isInspectionAvailable(this, expr) =>
      simplifications(expr).foreach {
        case s @ Simplification(toReplace, _, hint, rangeInParent) =>
          val quickFix = OperationOnCollectionQuickFix(s)
          holder.registerProblem(toReplace.getElement, hint, highlightType, rangeInParent, quickFix)
      }
  }

  private def simplifications(expr: ScExpression): Array[Simplification] = {
    def simplificationTypes = for {
      (st, idx) <- possibleSimplificationTypes.zipWithIndex
      if simplificationTypesEnabled(idx)
    } yield st

    simplificationTypes.flatMap(st => st.getSimplifications(expr) ++ st.getSimplification(expr))
  }

}
