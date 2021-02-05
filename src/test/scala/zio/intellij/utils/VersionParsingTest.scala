package zio.intellij.utils

import junit.framework.TestCase
import org.junit.Assert.{assertEquals, assertTrue}

class VersionParsingTest extends TestCase {

  private def assertVersion(
    version: Version,
    major: Version.Major,
    minor: Version.Minor,
    patch: Version.Patch,
    postfix: Option[Version.Postfix]
  ): Unit = {
    assertEquals(version.major, major)
    assertEquals(version.minor, minor)
    assertEquals(version.patch, patch)
    assertEquals(version.postfix, postfix)
  }

  def test_should_parse_simple_version(): Unit = {
    val versionOpt = Version.parse("1.2.3")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        Version.Major(1),
        Version.Minor(2),
        Version.Patch(3),
        None
      )
    }
  }

  def test_should_parse_rc_version(): Unit = {
    val versionOpt = Version.parse("1.2.3-RC4")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        Version.Major(1),
        Version.Minor(2),
        Version.Patch(3),
        Some(Version.RC(List(Version.PostfixSegment(4))))
      )
      assertEquals(version, Version.parseUnsafe("1.2.3-rc4"))
    }
  }

  def test_should_parse_rc_version_with_minor_part(): Unit = {
    val versionOpt = Version.parse("1.2.3-RC4-5")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        Version.Major(1),
        Version.Minor(2),
        Version.Patch(3),
        Some(Version.RC(List(4, 5).map(Version.PostfixSegment)))
      )
    }
  }

  def test_should_parse_extended_version(): Unit = {
    val versionOpt = Version.parse("1.0.4-2")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        Version.Major(1),
        Version.Minor(0),
        Version.Patch(4),
        Some(Version.Ext(List(Version.PostfixSegment(2))))
      )
    }
  }

  def test_should_parse_very_extended_version(): Unit = {
    val versionOpt = Version.parse("1.0.4-2-100-500-9000")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        Version.Major(1),
        Version.Minor(0),
        Version.Patch(4),
        Some(Version.Ext(List(2, 100, 500, 9000).map(Version.PostfixSegment)))
      )
    }
  }

  def test_should_fail_to_parse_version_without_minor_and_patch(): Unit =
    assertEquals(Version.parse("1"), None)

  def test_should_fail_to_parse_version_without_patch(): Unit =
    assertEquals(Version.parse("1.2"), None)

  def test_should_fail_to_parse_version_with_invalid_postfix_part(): Unit = {
    assertEquals(Version.parse("1.2.3-"), None)
    assertEquals(Version.parse("1.2.3-4-"), None)
    assertEquals(Version.parse("1.2.3-RC"), None)
    assertEquals(Version.parse("1.2.3-RC-"), None)
    assertEquals(Version.parse("1.2.3-RC-4-"), None)
  }

  def test_should_parse_versions_from_maven(): Unit =
    VersionTestUtils.zioVersionsFromMaven
      .foreach(versionStr => assertTrue(Version.parse(versionStr).isDefined))

}
