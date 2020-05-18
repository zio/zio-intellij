package zio.intellij.inspections

import javax.swing.JComponent
import org.jetbrains.plugins.scala.codeInspection.collections._

abstract class ZInspection(simplifiers: SimplificationType*) extends OperationOnCollectionInspection {
  final override def getLikeCollectionClasses: Array[String] = Array("zio.ZIO")

  final override def createOptionsPanel(): JComponent = null // god help me

  final override def possibleSimplificationTypes: Array[SimplificationType] = simplifiers.toArray
}
