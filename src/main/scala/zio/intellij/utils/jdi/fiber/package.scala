package zio.intellij.utils.jdi

import com.sun.jdi.{ObjectReference, Value}
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel
import zio.intellij.utils.Version
import zio.intellij.utils.jdi.fiber.model.ZTraceElement.{NoLocation, SourceLocation}
import zio.intellij.utils.jdi.fiber.model._

package object fiber {

  final case class RawFiberInfo(info: FiberInfo, children: List[Value])

  def convertFiberInfoWithChildren(
    value: Value
  )(implicit zioVersion: Version, languageLevel: ScalaLanguageLevel): Option[RawFiberInfo] =
    value match {
      case dump: ObjectReference if dump.`type`().name() == DumpRef.DumpName =>
        for {
          fiberId  <- convertFiberId(getFieldValue(dump, DumpRef.FiberIdField))
          fiberName = convertOption(getFieldValue(dump, DumpRef.FiberNameField))(convertStringValue)
          status   <- convertFiberStatus(getFieldValue(dump, DumpRef.StatusField))
          if status != FiberStatus.Done
          // in versions < rc19 there are no children and trace is not optional
          (children, trace) = if (zioVersion < Version.ZIO.RC19)
                                (Nil, convertZTrace(fiberId)(getFieldValue(dump, DumpRef.TraceField)))
                              else {
                                val children = convertScalaSeq(getFieldValue(dump, DumpRef.ChildrenField))
                                val trace =
                                  convertOption(getFieldValue(dump, DumpRef.TraceField))(convertZTrace(fiberId))
                                (children, trace)
                              }
        } yield RawFiberInfo(FiberInfo(fiberId, fiberName, status, trace), children)
      case _ => None
    }

  def convertFiberId(value: Value): Option[FiberId] =
    value match {
      case fiberId: ObjectReference if fiberId.`type`().name() == FiberIdRef.FiberIdName =>
        for {
          startTimeMillis <- convertLongValue(getFieldValue(fiberId, FiberIdRef.StartTimeMillisField))
          seqNumber       <- convertLongValue(getFieldValue(fiberId, FiberIdRef.SeqNumberField))
        } yield FiberId(startTimeMillis = startTimeMillis, seqNumber = seqNumber)
      case _ => None
    }

  def convertFiberStatus(value: Value)(implicit languageLevel: ScalaLanguageLevel): Option[FiberStatus] =
    value match {
      case status: ObjectReference =>
        status.`type`().name() match {
          case FiberStatus.DoneName => Some(FiberStatus.Done)
          case FiberStatus.RunningName =>
            val interrupting = convertBooleanValue(getFieldValue(status, FiberStatusRef.RunningRef.InterruptingField))
            interrupting.map(FiberStatus.Running)
          case FiberStatus.FinishingName =>
            val interrupting = convertBooleanValue(getFieldValue(status, FiberStatusRef.FinishingRef.InterruptingField))
            interrupting.map(FiberStatus.Finishing)
          case FiberStatus.SuspendedName =>
            for {
              interruptible <- convertBooleanValue(
                                 getFieldValue(status, FiberStatusRef.SuspendedRef.InterruptibleField)
                               )
              epoch <- convertLongValue(getFieldValue(status, FiberStatusRef.SuspendedRef.EpochField))
              blockingOn = convertScalaSeq(getFieldValue(status, FiberStatusRef.SuspendedRef.BlockingOnField))
                             .flatMap(convertFiberId)
              asyncTrace = convertOption(getFieldValue(status, FiberStatusRef.SuspendedRef.AsyncTraceField))(
                             convertZTraceElement
                           )
            } yield FiberStatus.Suspended(interruptible, epoch, blockingOn, asyncTrace)
          case _ => None
        }
      case _ => None
    }

  def convertZTraceElement(value: Value): Option[ZTraceElement] =
    value match {
      case srcLoc: ObjectReference if srcLoc.`type`().name() == ZTraceElementRef.SourceLocationName =>
        for {
          file   <- convertStringValue(getFieldValue(srcLoc, ZTraceElementRef.SourceLocationRef.FileField))
          clazz  <- convertStringValue(getFieldValue(srcLoc, ZTraceElementRef.SourceLocationRef.ClazzField))
          method <- convertStringValue(getFieldValue(srcLoc, ZTraceElementRef.SourceLocationRef.MethodField))
          line   <- convertIntegerValue(getFieldValue(srcLoc, ZTraceElementRef.SourceLocationRef.LineField))
        } yield SourceLocation(file, clazz, method, line)
      case noLoc: ObjectReference if noLoc.`type`().name() == ZTraceElementRef.NoLocationName =>
        val error = convertStringValue(getFieldValue(noLoc, ZTraceElementRef.NoLocationRef.ErrorField))
        error.map(NoLocation)
      case _ => None
    }

  def convertZTrace(fiberId: FiberId)(value: Value)(implicit languageLevel: ScalaLanguageLevel): Option[ZTrace] =
    value match {
      case trace: ObjectReference if trace.`type`().name() == ZTraceRef.ZTraceName =>
        val executionTrace =
          convertScalaSeq(getFieldValue(trace, ZTraceRef.ExecutionTraceField))
            .flatMap(convertZTraceElement)
        val stackTrace =
          convertScalaSeq(getFieldValue(trace, ZTraceRef.StackTraceField))
            .flatMap(convertZTraceElement)
        val parent = convertOption(getFieldValue(trace, ZTraceRef.ParentTraceField)) {
          case parentTrace: ObjectReference =>
            convertFiberId(getFieldValue(parentTrace, ZTraceRef.FiberIdField))
          case _ => None
        }
        Some(ZTrace(fiberId, executionTrace, stackTrace, parent))
      case _ => None
    }

  private[jdi] object DumpRef {
    // classes
    val DumpName = "zio.Fiber$Dump"

    // fields
    val FiberIdField   = "fiberId"
    val FiberNameField = "fiberName"
    val StatusField    = "status"
    val ChildrenField  = "children"
    val TraceField     = "trace"
  }

  private[jdi] object FiberIdRef {
    // classes
    val FiberIdName = "zio.Fiber$Id"

    // fields
    val StartTimeMillisField = "startTimeMillis"
    val SeqNumberField       = "seqNumber"
  }

  private[jdi] object FiberStatusRef {

    object RunningRef {
      // fields
      val InterruptingField = "interrupting"
    }

    object FinishingRef {
      // fields
      val InterruptingField = "interrupting"
    }

    object SuspendedRef {
      // fields
      val InterruptibleField = "interruptible"
      val EpochField         = "epoch"
      val BlockingOnField    = "blockingOn"
      val AsyncTraceField    = "asyncTrace"
    }
  }

  private[jdi] object ZTraceElementRef {
    // classes
    val SourceLocationName = "zio.internal.stacktracer.ZTraceElement$SourceLocation"
    val NoLocationName     = "zio.internal.stacktracer.ZTraceElement$NoLocation"

    object SourceLocationRef {
      // fields
      val FileField   = "file"
      val ClazzField  = "clazz"
      val MethodField = "method"
      val LineField   = "line"
    }

    object NoLocationRef {
      // fields
      val ErrorField = "error"
    }
  }

  private[jdi] object ZTraceRef {
    // classes
    val ZTraceName = "zio.ZTrace"

    // fields
    val ExecutionTraceField = "executionTrace"
    val StackTraceField     = "stackTrace"
    val ParentTraceField    = "parentTrace"
    val FiberIdField        = "fiberId"
  }

}
