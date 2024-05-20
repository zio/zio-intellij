package zio.intellij.inspections

import com.intellij.openapi.util.Key
import zio.intellij.inspections.macros.LayerBuilder.ZExpr

package object macros {
  val GraphDataKey: Key[LayerTree[ZExpr]] = Key.create[LayerTree[ZExpr]]("ZLayerGraphData")
}
