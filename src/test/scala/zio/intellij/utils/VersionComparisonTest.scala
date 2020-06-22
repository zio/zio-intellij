package zio.intellij.utils

import junit.framework.TestCase
import org.junit.Assert.{assertEquals, assertTrue}

class VersionComparisonTest extends TestCase {

  def test_equality(): Unit = {
    assertTrue(Version.parse("1.0.0").get === Version.parseUnsafe("1.0.0"))
    assertTrue(Version.parse("1.0.0").get >= Version.parseUnsafe("1.0.0"))
    assertTrue(Version.parse("1.0.0-RC1").get >= Version.parseUnsafe("1.0.0-RC1"))
    assertTrue(Version.parse("1.0.0-RC1-2").get <= Version.parseUnsafe("1.0.0-RC1-2"))
  }

  def test_gt(): Unit = {
    assertTrue(Version.parseUnsafe("1.0.0") > Version.parseUnsafe("0.0.0"))
    assertTrue(Version.parseUnsafe("1.0.0") > Version.parseUnsafe("0.0.1"))
    assertTrue(Version.parseUnsafe("1.0.0") > Version.parseUnsafe("0.1.0"))
    assertTrue(Version.parseUnsafe("1.2.0") > Version.parseUnsafe("1.1.0"))
    assertTrue(Version.parseUnsafe("1.2.3") > Version.parseUnsafe("1.2.2"))
  }

  def test_lt(): Unit = {
    assertTrue(Version.parseUnsafe("0.0.0") < Version.parseUnsafe("1.0.0"))
    assertTrue(Version.parseUnsafe("0.0.1") < Version.parseUnsafe("1.0.0"))
    assertTrue(Version.parseUnsafe("0.1.0") < Version.parseUnsafe("1.0.0"))
    assertTrue(Version.parseUnsafe("1.1.0") < Version.parseUnsafe("1.2.0"))
    assertTrue(Version.parseUnsafe("1.2.2") < Version.parseUnsafe("1.2.3"))
  }

  def test_rc_qt(): Unit = {
    assertTrue(Version.parseUnsafe("1.2.3-RC2") > Version.parseUnsafe("1.2.3-RC1"))
    assertTrue(Version.parseUnsafe("1.2.3-RC2-2") > Version.parseUnsafe("1.2.3-RC2-1"))
  }

  def test_rc_lt(): Unit = {
    assertTrue(Version.parseUnsafe("1.2.3-RC1") < Version.parseUnsafe("1.2.3-RC2"))
    assertTrue(Version.parseUnsafe("1.2.3-RC2-1") < Version.parseUnsafe("1.2.3-RC2-2"))
  }

  def test_rc_with_minor_part_should_be_greater_than_rc_without_minor(): Unit = {
    assertTrue(Version.parseUnsafe("1.2.3-RC2-2") > Version.parseUnsafe("1.2.3-RC2"))
    assertTrue(Version.parseUnsafe("1.2.3-RC2") < Version.parseUnsafe("1.2.3-RC2-2"))
  }

  def test_version_without_rc_should_be_greater_than_version_with_rc(): Unit = {
    assertTrue(Version.parseUnsafe("1.2.3") > Version.parseUnsafe("1.2.3-RC21"))
    assertTrue(Version.parseUnsafe("1.2.3") > Version.parseUnsafe("1.2.3-RC21-1"))

    assertTrue(Version.parseUnsafe("1.2.3-RC21") < Version.parseUnsafe("1.2.3"))
    assertTrue(Version.parseUnsafe("1.2.3-RC21-1") < Version.parseUnsafe("1.2.3"))
  }

  def test_sorting_on_zio_versions_from_maven(): Unit = {
    val versions       = VersionTestUtils.zioVersionsFromMaven.map(Version.parseUnsafe)
    val sortedVersions = versions.sorted(Version.versionOrdering.reverse)
    versions.reverse.zip(sortedVersions).foreach {
      case (v1, v2) => assertEquals(v1, v2)
    }
  }

}
