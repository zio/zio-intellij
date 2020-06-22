package zio.intellij.utils.jdi.fiber.model

/**
 * This class was copied from the zio project
 *
 * https://github.com/zio/zio/blob/e9124af60becf2cab93aa92cdab392975fb94664/core/shared/src/main/scala/zio/Fiber.scala#L510
 *
 * The identity of a Fiber, described by the time it began life, and a
 * monotonically increasing sequence number generated from an atomic counter.
 */
final case class FiberId(startTimeMillis: Long, seqNumber: Long) extends Serializable
