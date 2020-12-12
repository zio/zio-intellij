package zio.intellij.project

import com.intellij.openapi.ui.ValidationInfo
import com.intellij.platform.ProjectTemplate
import org.jetbrains.sbt.{Sbt, SbtBundle}
import zio.intellij.ZioIcon

import javax.swing.Icon

private[zio] class ZioProjectTemplate extends ProjectTemplate {
  //noinspection ReferencePassedToNls
  override def getName: String = "ZIO project (with sbt)"

  override def getDescription: String = "A new sbt project with the ZIO library."

  override def getIcon: Icon = ZioIcon

  override def createModuleBuilder() = new ZioProjectBuilder()

  override def validateSettings(): ValidationInfo = null
}
