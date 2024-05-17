package zio.intellij.utils

import junit.framework.TestCase
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel
import org.junit.Assert._

class VersionComparisonTest extends TestCase {

  def test_equality(): Unit = {
    assertTrue(ZioVersion.parse("1.0.0").get === ZioVersion.parseUnsafe("1.0.0"))
    assertTrue(ZioVersion.parse("1.0.0").get >= ZioVersion.parseUnsafe("1.0.0"))
    assertTrue(ZioVersion.parse("1.0.0-RC1").get >= ZioVersion.parseUnsafe("1.0.0-RC1"))
    assertTrue(ZioVersion.parse("1.0.0-RC1-2").get <= ZioVersion.parseUnsafe("1.0.0-RC1-2"))
    assertTrue(ZioVersion.parse("1.0.0-M1").get >= ZioVersion.parseUnsafe("1.0.0-M1"))
    assertTrue(ZioVersion.parse("1.0.0-M1-2").get <= ZioVersion.parseUnsafe("1.0.0-M1-2"))
  }

  def test_gt(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.0.0") > ZioVersion.parseUnsafe("0.0.0"))
    assertTrue(ZioVersion.parseUnsafe("1.0.0") > ZioVersion.parseUnsafe("0.0.1"))
    assertTrue(ZioVersion.parseUnsafe("1.0.0") > ZioVersion.parseUnsafe("0.1.0"))
    assertTrue(ZioVersion.parseUnsafe("1.2.0") > ZioVersion.parseUnsafe("1.1.0"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3") > ZioVersion.parseUnsafe("1.2.2"))
  }

  def test_lt(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("0.0.0") < ZioVersion.parseUnsafe("1.0.0"))
    assertTrue(ZioVersion.parseUnsafe("0.0.1") < ZioVersion.parseUnsafe("1.0.0"))
    assertTrue(ZioVersion.parseUnsafe("0.1.0") < ZioVersion.parseUnsafe("1.0.0"))
    assertTrue(ZioVersion.parseUnsafe("1.1.0") < ZioVersion.parseUnsafe("1.2.0"))
    assertTrue(ZioVersion.parseUnsafe("1.2.2") < ZioVersion.parseUnsafe("1.2.3"))
  }

  def test_rc_gt(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC2") > ZioVersion.parseUnsafe("1.2.3-RC1"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC2-2") > ZioVersion.parseUnsafe("1.2.3-RC2-1"))
  }

  def test_rc_lt(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC1") < ZioVersion.parseUnsafe("1.2.3-RC2"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC2-1") < ZioVersion.parseUnsafe("1.2.3-RC2-2"))
  }

  def test_rc_with_minor_part_should_be_greater_than_rc_without_minor(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC2-2") > ZioVersion.parseUnsafe("1.2.3-RC2"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC2") < ZioVersion.parseUnsafe("1.2.3-RC2-2"))
  }

  def test_milestone_gt(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M2") > ZioVersion.parseUnsafe("1.2.3-M1"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M2-2") > ZioVersion.parseUnsafe("1.2.3-M2-1"))
  }

  def test_milestone_lt(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M1") < ZioVersion.parseUnsafe("1.2.3-M2"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M2-1") < ZioVersion.parseUnsafe("1.2.3-M2-2"))
  }

  def test_milestone_with_minor_part_should_be_greater_than_milestone_without_minor(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M2-2") > ZioVersion.parseUnsafe("1.2.3-M2"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M2") < ZioVersion.parseUnsafe("1.2.3-M2-2"))
  }

  def test_version_without_postfix_should_be_greater_than_version_with_rc(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3") > ZioVersion.parseUnsafe("1.2.3-RC21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3") > ZioVersion.parseUnsafe("1.2.3-RC21-1"))

    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC21") < ZioVersion.parseUnsafe("1.2.3"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC21-1") < ZioVersion.parseUnsafe("1.2.3"))
  }

  def test_version_without_postfix_should_be_greater_than_version_with_milestone(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3") > ZioVersion.parseUnsafe("1.2.3-M21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3") > ZioVersion.parseUnsafe("1.2.3-M21-1"))

    assertTrue(ZioVersion.parseUnsafe("1.2.3-M21") < ZioVersion.parseUnsafe("1.2.3"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M21-1") < ZioVersion.parseUnsafe("1.2.3"))
  }

  def test_version_with_rc_should_be_greater_than_version_with_milestone(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC21") > ZioVersion.parseUnsafe("1.2.3-M21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC21-1") > ZioVersion.parseUnsafe("1.2.3-M21-1"))

    assertTrue(ZioVersion.parseUnsafe("1.2.3-M21") < ZioVersion.parseUnsafe("1.2.3-RC21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M21-1") < ZioVersion.parseUnsafe("1.2.3-RC21-1"))
  }

  def test_version_without_postfix_should_be_less_than_version_with_ext(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3") < ZioVersion.parseUnsafe("1.2.3-21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3") < ZioVersion.parseUnsafe("1.2.3-21-1"))

    assertTrue(ZioVersion.parseUnsafe("1.2.3-21") > ZioVersion.parseUnsafe("1.2.3"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-21-1") > ZioVersion.parseUnsafe("1.2.3"))
  }

  def test_version_with_ext_should_be_greater_than_version_with_rc(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-21") > ZioVersion.parseUnsafe("1.2.3-RC21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-21-1") > ZioVersion.parseUnsafe("1.2.3-RC21-1"))

    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC21") < ZioVersion.parseUnsafe("1.2.3-21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-RC21-1") < ZioVersion.parseUnsafe("1.2.3-21-1"))
  }

  def test_version_with_ext_should_be_greater_than_version_with_milestone(): Unit = {
    assertTrue(ZioVersion.parseUnsafe("1.2.3-21") > ZioVersion.parseUnsafe("1.2.3-M21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-21-1") > ZioVersion.parseUnsafe("1.2.3-M21-1"))

    assertTrue(ZioVersion.parseUnsafe("1.2.3-M21") < ZioVersion.parseUnsafe("1.2.3-21"))
    assertTrue(ZioVersion.parseUnsafe("1.2.3-M21-1") < ZioVersion.parseUnsafe("1.2.3-21-1"))
  }

  def test_scala_3_prerelease_version_comparison(): Unit = {
    assertFalse(ZioVersion.scala3Version.isPrerelease)
    val prereleaseScala3Version = new ScalaVersion(ScalaLanguageLevel.Scala_3_0, "0-RC2")
    assertTrue(prereleaseScala3Version.isPrerelease)
    assertTrue(prereleaseScala3Version < ZioVersion.scala3Version)
    assertTrue(ZioVersion.scala3Version > prereleaseScala3Version)
  }

  def test_sorting_on_zio_versions_from_maven(): Unit = {
    val versions       = VersionTestUtils.zioVersionsFromMaven.map(ZioVersion.parseUnsafe)
    val sortedVersions = versions.sorted(ZioVersion.versionOrdering.reverse)
    versions.reverse.zip(sortedVersions).foreach {
      case (v1, v2) => assertEquals(v1, v2)
    }
  }

}
