package zio.intellij.utils

import zio.intellij.utils.Version._

import scala.util.matching.Regex

sealed abstract case class Version private (major: Major, minor: Minor, patch: Patch, rcVersion: Option[RCVersion])
    extends Ordered[Version] {
  def ===(that: Version): Boolean = Version.versionOrdering.equiv(this, that)

  override def compare(that: Version): Int =
    Version.versionOrdering.compare(this, that)

  override def toString: String =
    s"${major.value}.${minor.value}.${patch.value}${rcVersion.fold("")(rc => s"-${rc.toString}")}"
}

object Version {

  val versionOrdering: Ordering[Version] =
    Ordering[(Major, Minor, Patch)]
      .on[Version](v => (v.major, v.minor, v.patch))
      // 1.0.0-RC1 should be less than 1.0.0
      .orElse(Ordering.Option(Ordering[RCVersion].reverse).reverse.on[Version](_.rcVersion))

  final case class Major(value: Int) extends Ordered[Major] {
    override def compare(that: Major): Int = this.value.compare(that.value)
  }

  final case class Minor(value: Int) extends Ordered[Minor] {
    override def compare(that: Minor): Int = this.value.compare(that.value)
  }

  final case class Patch(value: Int) extends Ordered[Patch] {
    override def compare(that: Patch): Int = this.value.compare(that.value)
  }

  final case class RCVersion(major: RCMajor, minor: Option[RCMinor]) extends Ordered[RCVersion] {
    override def compare(that: RCVersion): Int = RCVersion.ordering.compare(this, that)

    override def toString: String = s"RC${major.value}${minor.fold("")(m => s"-${m.value}")}"
  }

  object RCVersion {

    val ordering: Ordering[RCVersion] =
      // For now, we don't care if RC1-0 is greater thar RC1
      // But RC1-2 should be greater than RC1
      Ordering[(RCMajor, Option[RCMinor])].on[RCVersion](v => (v.major, v.minor))
  }

  final case class RCMajor(value: Int) extends Ordered[RCMajor] {
    override def compare(that: RCMajor): Int = this.value.compare(that.value)
  }

  final case class RCMinor(value: Int) extends Ordered[RCMinor] {
    override def compare(that: RCMinor): Int = this.value.compare(that.value)
  }

  val versionRegex: Regex = """(\d+).(\d+).(\d+)(?:(?:-(?:(?:RC)|(?:rc))(\d+))(?:-(\d+))?)?""".r

  def parse(str: String): Option[Version] =
    str match {
      case versionRegex(majorStr, minorStr, patchStr, rcMajorStr, rcMinorStr) =>
        val major        = Major(majorStr.toInt)
        val minor        = Minor(minorStr.toInt)
        val patch        = Patch(patchStr.toInt)
        val rcMajorOpt   = Option(rcMajorStr).map(rcMajorStr => RCMajor(rcMajorStr.toInt))
        val rcMinorOpt   = Option(rcMinorStr).map(rcMinorStr => RCMinor(rcMinorStr.toInt))
        val rcVersionOpt = rcMajorOpt.map(RCVersion(_, rcMinorOpt))

        Some(new Version(major, minor, patch, rcVersionOpt) {})
      case _ => None
    }

  def parseUnsafe(str: String): Version =
    parse(str).getOrElse(throw new IllegalArgumentException(s"Could not parse version: $str"))

  object ZIO {
    val RC18: Version     = Version.parseUnsafe("1.0.0-RC18")
    val RC19: Version     = Version.parseUnsafe("1.0.0-RC19")
    val RC21: Version     = Version.parseUnsafe("1.0.0-RC21")
    val `RC21-2`: Version = Version.parseUnsafe("1.0.0-RC21-2")
  }

}
