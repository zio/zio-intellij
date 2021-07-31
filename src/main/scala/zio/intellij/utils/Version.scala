package zio.intellij.utils

import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel
import zio.intellij.utils.Version._

import scala.annotation.tailrec
import scala.util.matching.Regex

sealed abstract case class Version private (major: Major, minor: Minor, patch: Patch, postfix: Option[Postfix])
    extends Ordered[Version] {
  def ===(that: Version): Boolean = Version.versionOrdering.equiv(this, that)

  override def compare(that: Version): Int =
    Version.versionOrdering.compare(this, that)

  override def toString: String =
    s"${major.value}.${minor.value}.${patch.value}${postfix.fold("")("-" + _)}"
}

object Version {
  val scala3Version = new ScalaVersion(ScalaLanguageLevel.Scala_3_0, "0")

  val versionOrdering: Ordering[Version] = (x: Version, y: Version) => {
    val compareWithoutPostfix = (x.major, x.minor, x.patch).compare((y.major, y.minor, y.patch))
    if (compareWithoutPostfix != 0) compareWithoutPostfix
    else {
      (x.postfix, y.postfix) match {
        case (None, None) => 0
        // '1.0.4-M1' < '1.0.4-RC1' < '1.0.4' < '1.0.4-1'   (╯°□°)╯︵ ┻━┻
        case (None, Some(Ext(_))) => -1
        case (None, Some(_))      => 1
        case (Some(Ext(_)), None) => 1
        case (Some(_), None)      => -1
        case (Some(p1), Some(p2)) => p1.compare(p2)
      }
    }
  }

  final case class Major(value: Int) extends Ordered[Major] {
    override def compare(that: Major): Int = this.value.compare(that.value)
  }

  final case class Minor(value: Int) extends Ordered[Minor] {
    override def compare(that: Minor): Int = this.value.compare(that.value)
  }

  final case class Patch(value: Int) extends Ordered[Patch] {
    override def compare(that: Patch): Int = this.value.compare(that.value)
  }

  final case class PostfixSegment(value: Int) extends Ordered[PostfixSegment] {
    override def compare(that: PostfixSegment): Int = this.value.compare(that.value)
  }

  sealed trait Postfix extends Ordered[Postfix] {
    def segments: List[PostfixSegment]

    override def toString: String = segments.map(_.value).mkString("-")

    override def compare(that: Postfix): Int = (this, that) match {
      case (Milestone(_), RC(_))  => -1
      case (Milestone(_), Ext(_)) => -1
      case (RC(_), Ext(_))        => -1
      case (Ext(_), RC(_))        => 1
      case (Ext(_), Milestone(_)) => 1
      case (RC(_), Milestone(_))  => 1
      case _                      => compareSegments(this.segments, that.segments)
    }

    @tailrec
    private def compareSegments(left: List[PostfixSegment], right: List[PostfixSegment]): Int =
      (left, right) match {
        case (Nil, Nil)         => 0
        case (Nil, _ :: _)      => -1
        case (_ :: _, Nil)      => 1
        case (l :: ls, r :: rs) => if (l == r) compareSegments(ls, rs) else l.compare(r)
      }
  }

  final case class RC(segments: List[PostfixSegment]) extends Postfix {
    override def toString: String = s"RC${super.toString}"
  }

  final case class Milestone(segments: List[PostfixSegment]) extends Postfix {
    override def toString: String = s"M${super.toString}"
  }

  final case class Ext(segments: List[PostfixSegment]) extends Postfix

  // TODO: Get rid of this horror...
  private val versionRegex: Regex = """(\d+).(\d+).(\d+)((?:-((?:RC)|(?:rc)|(?:M)|(?:m))?\d+)(?:-\d+)*)?""".r
  private val numericRegex: Regex = """\d+""".r

  def parse(str: String): Option[Version] =
    str match {
      case versionRegex(majorStr, minorStr, patchStr, postfixStr, rcOrMStr) =>
        val major = Major(majorStr.toInt)
        val minor = Minor(minorStr.toInt)
        val patch = Patch(patchStr.toInt)

        val postfix = Option(postfixStr).map { postfix =>
          val segments = numericRegex.findAllIn(postfix).toList.map(x => PostfixSegment(x.toInt))

          Option(rcOrMStr).map(_.toLowerCase) match {
            case Some("m")  => Milestone(segments)
            case Some("rc") => RC(segments)
            case _          => Ext(segments)
          }
        }

        Some(new Version(major, minor, patch, postfix) {})
      case _ => None
    }

  def parseUnsafe(str: String): Version =
    parse(str).getOrElse(throw new IllegalArgumentException(s"Could not parse version: $str"))

  object ZIO {
    val RC18: Version     = Version.parseUnsafe("1.0.0-RC18")
    val `RC18-2`: Version = Version.parseUnsafe("1.0.0-RC18-2")
    val RC19: Version     = Version.parseUnsafe("1.0.0-RC19")
    val RC21: Version     = Version.parseUnsafe("1.0.0-RC21")
    val `RC21-2`: Version = Version.parseUnsafe("1.0.0-RC21-2")
    val `1.0.0`: Version  = Version.parseUnsafe("1.0.0")
    val `1.0.6`: Version  = Version.parseUnsafe("1.0.6")
    val `1.0.8`: Version  = Version.parseUnsafe("1.0.8") // first version to support Scala 3.0.0

    val `latest-ish`: Version = Version.parseUnsafe("1.0.10")
  }
}
