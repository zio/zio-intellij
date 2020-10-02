package zio.intellij.utils.types

import zio.intellij.utils.types.ZLayerType.Name

sealed class ZLayerType(val name: String) extends Type {
  val fqName = s"zio.$name"
}

object ZLayerType {
  def unapply(t: ZLayerType): Option[String] = Some(t.name)

  object Name {
    val ZLayer    = "ZLayer"
    val RLayer    = "RLayer"
    val URLayer   = "URLayer"
    val Layer     = "Layer"
    val ULayer    = "ULayer"
    val TaskLayer = "TaskLayer"
  }
}

object ZLayerTypes extends TypeCompanion[ZLayerType] {
  val ZLayer    = new ZLayerType(Name.ZLayer)
  val RLayer    = new ZLayerType(Name.RLayer)
  val URLayer   = new ZLayerType(Name.URLayer)
  val Layer     = new ZLayerType(Name.Layer)
  val ULayer    = new ZLayerType(Name.ULayer)
  val TaskLayer = new ZLayerType(Name.TaskLayer)

  override val defaultValue: ZLayerType = ZLayer

  override val values = List(
    ZLayer,
    RLayer,
    URLayer,
    Layer,
    ULayer,
    TaskLayer
  )
}
