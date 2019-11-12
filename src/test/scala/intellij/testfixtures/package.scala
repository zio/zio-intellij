package intellij

import org.jetbrains.plugins.scala.DependencyManagerBase.DependencyDescription
import org.jetbrains.plugins.scala.ScalaVersion

package object testfixtures {
  implicit class RichStr(private val org: String) extends AnyVal {

    def %(artId: String) = DependencyDescription(org, artId, "")

    def %%(artId: String)(implicit scalaVersion: ScalaVersion) = DependencyDescription(
      org,
      artId + "_" + scalaVersion.major,
      ""
    )
  }
}
