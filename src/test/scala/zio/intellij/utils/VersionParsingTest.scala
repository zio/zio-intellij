package zio.intellij.utils

import junit.framework.TestCase
import org.junit.Assert.{assertEquals, assertTrue}

class VersionParsingTest extends TestCase {

  private def assertVersion(
    version: ZioVersion,
    major: ZioVersion.Major,
    minor: ZioVersion.Minor,
    patch: ZioVersion.Patch,
    postfix: Option[ZioVersion.Postfix]
  ): Unit = {
    assertEquals(version.major, major)
    assertEquals(version.minor, minor)
    assertEquals(version.patch, patch)
    assertEquals(version.postfix, postfix)
  }

  def test_should_parse_simple_version(): Unit = {
    val versionOpt = ZioVersion.parse("1.2.3")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(2),
        ZioVersion.Patch(3),
        None
      )
    }
  }

  def test_should_parse_rc_version(): Unit = {
    val versionOpt = ZioVersion.parse("1.2.3-RC4")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(2),
        ZioVersion.Patch(3),
        Some(ZioVersion.RC(List(ZioVersion.PostfixSegment(4))))
      )
      assertEquals(version, ZioVersion.parseUnsafe("1.2.3-rc4"))
    }
  }

  def test_should_parse_milestone_version(): Unit = {
    val versionOpt = ZioVersion.parse("1.2.3-M4")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(2),
        ZioVersion.Patch(3),
        Some(ZioVersion.Milestone(List(ZioVersion.PostfixSegment(4))))
      )
      assertEquals(version, ZioVersion.parseUnsafe("1.2.3-m4"))
    }
  }

  def test_should_parse_rc_version_with_minor_part(): Unit = {
    val versionOpt = ZioVersion.parse("1.2.3-RC4-5")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(2),
        ZioVersion.Patch(3),
        Some(ZioVersion.RC(List(4, 5).map(ZioVersion.PostfixSegment)))
      )
    }
  }

  def test_should_parse_milestone_version_with_minor_part(): Unit = {
    val versionOpt = ZioVersion.parse("1.2.3-M4-5")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(2),
        ZioVersion.Patch(3),
        Some(ZioVersion.Milestone(List(4, 5).map(ZioVersion.PostfixSegment)))
      )
    }
  }

  def test_should_parse_extended_version(): Unit = {
    val versionOpt = ZioVersion.parse("1.0.4-2")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(0),
        ZioVersion.Patch(4),
        Some(ZioVersion.Ext(List(ZioVersion.PostfixSegment(2))))
      )
    }
  }

  def test_should_parse_very_extended_version(): Unit = {
    val versionOpt = ZioVersion.parse("1.0.4-2-100-500-9000")
    assertTrue(versionOpt.isDefined)
    versionOpt.foreach { version =>
      assertVersion(
        version,
        ZioVersion.Major(1),
        ZioVersion.Minor(0),
        ZioVersion.Patch(4),
        Some(ZioVersion.Ext(List(2, 100, 500, 9000).map(ZioVersion.PostfixSegment)))
      )
    }
  }

  def test_should_fail_to_parse_version_without_minor_and_patch(): Unit =
    assertEquals(ZioVersion.parse("1"), None)

  def test_should_fail_to_parse_version_without_patch(): Unit =
    assertEquals(ZioVersion.parse("1.2"), None)

  def test_should_fail_to_parse_version_with_invalid_postfix_part(): Unit = {
    assertEquals(ZioVersion.parse("1.2.3-"), None)
    assertEquals(ZioVersion.parse("1.2.3-4-"), None)
    assertEquals(ZioVersion.parse("1.2.3-RC"), None)
    assertEquals(ZioVersion.parse("1.2.3-M"), None)
    assertEquals(ZioVersion.parse("1.2.3-RC-"), None)
    assertEquals(ZioVersion.parse("1.2.3-M-"), None)
    assertEquals(ZioVersion.parse("1.2.3-RC-4-"), None)
    assertEquals(ZioVersion.parse("1.2.3-M-4-"), None)
  }

  def test_should_parse_versions_from_maven(): Unit =
    VersionTestUtils.zioVersionsFromMaven
      .foreach(versionStr => assertTrue(ZioVersion.parse(versionStr).isDefined))

}
