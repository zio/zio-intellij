package zio.intellij.utils.types

import zio.intellij.utils.types.ZioType.Name

sealed class ZioType(val name: String) extends Type {
  val fqName = s"zio.$name"
}

object ZioType {
  def unapply(t: ZioType): Option[String] = Some(t.name)

  object Name {
    val ZIO  = "ZIO"
    val UIO  = "UIO"
    val RIO  = "RIO"
    val URIO = "URIO"
    val IO   = "IO"
    val Task = "Task"
  }
}

object ZioTypes extends TypeCompanion[ZioType] {
  val ZIO  = new ZioType(Name.ZIO)
  val UIO  = new ZioType(Name.UIO)
  val RIO  = new ZioType(Name.RIO)
  val URIO = new ZioType(Name.URIO)
  val IO   = new ZioType(Name.IO)
  val Task = new ZioType(Name.Task)

  override val defaultValue: ZioType = ZIO

  override val values = List(
    ZIO,
    UIO,
    RIO,
    URIO,
    IO,
    Task
  )
}
