package zio.intellij

import com.intellij.openapi.util.Iconable
import javax.swing.Icon

package object intentions {

  trait ZIcon extends Iconable {
    override def getIcon(flags: Int): Icon = zio.intellij.ZioIcon
  }
}
