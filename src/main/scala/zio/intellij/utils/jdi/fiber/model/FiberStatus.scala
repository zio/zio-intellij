package zio.intellij.utils.jdi.fiber.model

/**
 * This trait was copied from the zio project and modified
 *
 * https://github.com/zio/zio/blob/e9124af60becf2cab93aa92cdab392975fb94664/core/shared/src/main/scala/zio/Fiber.scala#L519
 */
sealed trait FiberStatus extends Product with Serializable { self =>
  import FiberStatus._

  val name: String = self match {
    case Done         => "Done"
    case _: Running   => "Running"
    case _: Finishing => "Finishing"
    case _: Suspended => "Suspended"
  }
}

object FiberStatus {
  private val classPrefix   = "zio.Fiber$Status$"
  val RunningName: String   = classPrefix + "Running"
  val SuspendedName: String = classPrefix + "Suspended"
  val FinishingName: String = classPrefix + "Finishing"
  val DoneName: String      = classPrefix + "Done$"

  implicit val ordering: Ordering[FiberStatus] = {
    case (x, y) if x eq y  => 0
    case (_: Running, _)   => -1
    case (_, _: Running)   => 1
    case (_: Suspended, _) => -1
    case (_, _: Suspended) => 1
    case (_: Finishing, _) => -1
    case (_, _: Finishing) => 1
  }

  case object Done                                  extends FiberStatus
  final case class Finishing(interrupting: Boolean) extends FiberStatus
  final case class Running(interrupting: Boolean)   extends FiberStatus

  final case class Suspended(
    interruptible: Boolean,
    epoch: Long,
    blockingOn: List[FiberId],
    asyncTrace: Option[ZTraceElement]
  ) extends FiberStatus
}
