package zio.intellij

import com.intellij.ui.IconManager
import javax.swing.Icon

package object icons {
  private def load(path: String): Icon = IconManager.getInstance.getIcon(path, this.getClass.getClassLoader)

  val SortByIdIcon: Icon = load("/icons/sortById.svg")
}
