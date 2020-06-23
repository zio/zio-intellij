package zio.intellij.utils.jdi.fiber.model

/**
 * This class was copied from the zio project and modified
 *
 * https://github.com/zio/zio/blob/e9124af60becf2cab93aa92cdab392975fb94664/core/shared/src/main/scala/zio/ZTrace.scala#L23
 */
final case class ZTrace(
  fiberId: FiberId,
  executionTrace: List[ZTraceElement],
  stackTrace: List[ZTraceElement],
  parent: Option[FiberId]
) {

  def prettyPrint: String = {
    val hasExecTrace  = executionTrace.nonEmpty
    val hasStackTrace = stackTrace.nonEmpty

    val stackPrint =
      if (hasStackTrace)
        s"Fiber#${fiberId.seqNumber} was supposed to continue to:" ::
          stackTrace.map(loc => s"  a future continuation at " + loc.prettyPrint)
      else
        s"Fiber#${fiberId.seqNumber} was supposed to continue to: <empty trace>" :: Nil

    val execPrint =
      if (hasExecTrace)
        s"Fiber#${fiberId.seqNumber} execution trace:" ::
          executionTrace.map(loc => "  at " + loc.prettyPrint)
      else s"Fiber#${fiberId.seqNumber} ZIO Execution trace: <empty trace>" :: Nil

    val ancestry =
      parent.map(parentId => s"Fiber#${fiberId.seqNumber} was spawned by: #${parentId.seqNumber}" :: Nil).getOrElse(Nil)

    (stackPrint ++ execPrint ++ ancestry).mkString("\n")
  }

}
