package zio.intellij.utils.jdi.fiber.model

final case class FiberInfo(id: FiberId, name: Option[String], status: FiberStatus, trace: Option[ZTrace]) {
  val renderName: String = s"${name.getOrElse("unnamed")}-${id.startTimeMillis}@${id.seqNumber}"
}
