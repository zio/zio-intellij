package zio.intellij.ui

import zio.intellij.utils.jdi.fiber.model.{FiberInfo, FiberStatus}

object FiberDumpRenderer {

  /**
   * This function was copied from the zio project and modified
   *
   * https://github.com/zio/zio/blob/e9124af60becf2cab93aa92cdab392975fb94664/core/shared/src/main/scala/zio/internal/FiberRenderer.scala#L19
   */
  def renderFiberInfo(fiberInfo: FiberInfo): String = {
    val name = fiberInfo.name.fold("")(name => "\"" + name + "\"")
    val waitMsg = fiberInfo.status match {
      case FiberStatus.Suspended(_, _, blockingOn, _) if blockingOn.nonEmpty =>
        "waiting on " + blockingOn.map(id => s"#${id.seqNumber}").mkString(", ")
      case _ => ""
    }
    val statusMsg = fiberInfo.status match {
      case FiberStatus.Done         => "Done"
      case FiberStatus.Finishing(b) => "Finishing" + (if (b) "(interrupting)" else "")
      case FiberStatus.Running(b)   => "Running" + (if (b) "(interrupting)" else "")
      case FiberStatus.Suspended(interruptible, epoch, _, asyncTrace) =>
        val in = if (interruptible) "interruptible" else "uninterruptible"
        val ep = s"$epoch asyncs"
        val as = asyncTrace.map(_.prettyPrint).mkString(" ")
        s"Suspended($in, $ep, $as)"
    }

    s"""$name#${fiberInfo.id.seqNumber} $waitMsg
       |   Status: $statusMsg
       |${fiberInfo.trace.fold("")(_.prettyPrint)}
       |""".stripMargin
  }
}
