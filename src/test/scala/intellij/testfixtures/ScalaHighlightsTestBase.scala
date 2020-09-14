package org.jetbrains.plugins.scala
package codeInspection

import com.intellij.codeHighlighting.HighlightDisplayLevel
import com.intellij.codeInsight.daemon.impl.HighlightInfo
import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInspection.ex.ScopeToolState
import com.intellij.codeInspection.{LocalInspectionEP, LocalInspectionTool}
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.util.TextRange
import com.intellij.profile.codeInspection.ProjectInspectionProfileManager
import com.intellij.psi.PsiFile
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.testFramework.EditorTestUtil
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter.{findCaretOffset, normalize}
import org.jetbrains.plugins.scala.extensions.executeWriteActionCommand
import org.junit.Assert._

import scala.collection.JavaConverters

abstract class ScalaHighlightsTestBase extends ScalaLightCodeInsightFixtureTestAdapter {
  self: ScalaLightCodeInsightFixtureTestAdapter =>

  import ScalaHighlightsTestBase._

  protected val description: String

  protected val fileType: LanguageFileType = ScalaFileType.INSTANCE

  protected def descriptionMatches(s: String): Boolean = s == normalize(description)

  override protected def checkTextHasNoErrors(text: String): Unit = {
    val ranges = findRanges(text)
    assertTrue(
      if (shouldPass) s"Highlights found at: ${ranges.mkString(", ")}." else failingPassed,
      !shouldPass ^ ranges.isEmpty
    )
  }

  protected def checkTextHasError(text: String, allowAdditionalHighlights: Boolean = false): Unit = {
    val expectedRanges = selectedRanges(normalize(text))
    val actualRanges   = findRanges(text)
    checkTextHasError(expectedRanges, actualRanges, allowAdditionalHighlights)
  }

  protected def checkTextHasError(
    expectedHighlightRanges: Seq[TextRange],
    actualHighlightRanges: Seq[TextRange],
    allowAdditionalHighlights: Boolean
  ): Unit = {
    val expectedRangesNotFound = expectedHighlightRanges.filterNot(actualHighlightRanges.contains)
    if (shouldPass) {
      assertTrue(s"Highlights not found: $description", actualHighlightRanges.nonEmpty)
      assertTrue(
        s"Highlights found at: ${actualHighlightRanges.mkString(", ")}, " +
          s"not found: ${expectedRangesNotFound.mkString(", ")}",
        expectedRangesNotFound.isEmpty
      )
      val duplicatedHighlights = actualHighlightRanges
        .groupBy(identity)
        .mapValues(_.length)
        .toSeq
        .collect { case (highlight, count) if count > 1 => highlight }

      assertTrue(
        s"Some highlights were duplicated: ${duplicatedHighlights.mkString(", ")}",
        duplicatedHighlights.isEmpty
      )
      if (!allowAdditionalHighlights) {
        assertTrue(
          s"Found too many highlights: ${actualHighlightRanges.mkString(", ")}, " +
            s"expected: ${expectedHighlightRanges.mkString(", ")}",
          actualHighlightRanges.length == expectedHighlightRanges.length
        )
      }
    } else {
      assertTrue(failingPassed, actualHighlightRanges.isEmpty)
      assertFalse(failingPassed, expectedRangesNotFound.isEmpty)
    }
  }

  protected def findRanges(text: String): Seq[TextRange] = configureByText(text).map(_._2)

  protected def configureByText(text: String): Seq[(HighlightInfo, TextRange)] = {
    val fileText                 = createTestText(text)
    val (normalizedText, offset) = findCaretOffset(fileText, stripTrailingSpaces = true)

    val fixture = getFixture
    fixture.configureByText(fileType, normalizedText)

    import JavaConverters._
    val highlightInfos = fixture
      .doHighlighting()
      .asScala
      .filter(it => descriptionMatches(it.getDescription))
    highlightInfos
      .map(info => (info, highlightedRange(info)))
      .filter(checkOffset(_, offset))
  }

  protected def createTestText(text: String): String = text

  protected def selectedRanges(text: String): Seq[TextRange] = {
    import JavaConverters._
    val document = EditorFactory.getInstance.createDocument(text)
    val state    = EditorTestUtil.extractCaretAndSelectionMarkers(document)
    state.carets.asScala.toList.map(_.selection)
  }

  protected def reformatFile(file: PsiFile): Unit =
    executeWriteActionCommand() {
      CodeStyleManager.getInstance(getProject).reformat(file)
    }(getProject)
}

object ScalaHighlightsTestBase {

  private def highlightedRange(info: HighlightInfo): TextRange =
    new TextRange(info.getStartOffset, info.getEndOffset)

  private def checkOffset(pair: (HighlightInfo, TextRange), offset: Int): Boolean = pair match {
    case _ if offset == -1 => true
    case (_, range)        => range.containsOffset(offset)
  }
}

abstract class ScalaQuickFixTestBase extends ScalaInspectionTestBase

abstract class ScalaAnnotatorQuickFixTestBase extends ScalaHighlightsTestBase {
  import ScalaAnnotatorQuickFixTestBase.quickFixes

  protected def testQuickFixes(text: String, expected: String, hint: String): Unit = {
    val actions = findAllQuickFixes(text, hint)
    assertTrue(s"Quick fixes not found: $hint", actions.nonEmpty)

    executeWriteActionCommand() {
      actions.foreach(_.invoke(getProject, getEditor, getFile))
    }(getProject)

    val expectedFile = createLightFile(ScalaFileType.INSTANCE, normalize(createTestText(expected)))

    reformatFile(getFile)
    reformatFile(expectedFile)

    getFixture.checkResult(expectedFile.getText, /*stripTrailingSpaces = */ true)
  }

  protected def checkNotFixable(text: String, hint: String): Unit = {
    val maybeAction = findQuickFix(text, hint)
    assertTrue("Quick fix found.", maybeAction.isEmpty)
  }

  protected def checkIsNotAvailable(text: String, hint: String): Unit = {
    val actions = findAllQuickFixes(text, hint)
    assertTrue("Quick fix not found.", actions.nonEmpty)
    assertTrue(
      "Quick fix is available",
      actions.forall(action => !action.isAvailable(getProject, getEditor, getFile))
    )
  }

  private def findQuickFix(text: String, hint: String): Option[IntentionAction] =
    findAllQuickFixes(text, hint).headOption

  private def findAllQuickFixes(text: String, hint: String): Seq[IntentionAction] =
    configureByText(text).map(_._1) match {
      case Seq() =>
        fail("Errors not found.")
        Nil
      case seq => seq.flatMap(quickFixes).filter(_.getText == hint)
    }
}

object ScalaAnnotatorQuickFixTestBase {

  private def quickFixes(info: HighlightInfo): Seq[IntentionAction] = {
    import JavaConverters._
    Option(info.quickFixActionRanges).toSeq
      .flatMap(_.asScala)
      .flatMap(pair => Option(pair))
      .map(_.getFirst.getAction)
  }
}

abstract class ScalaInspectionTestBase extends ScalaAnnotatorQuickFixTestBase {

  protected val classOfInspection: Class[_ <: LocalInspectionTool]

  override protected def setUp(): Unit = {
    super.setUp()
    getFixture.enableInspections(classOfInspection)
  }
}

trait ForceInspectionSeverity extends ScalaInspectionTestBase {

  private var oldLevel: HighlightDisplayLevel = _

  override protected def setUp(): Unit = {
    super.setUp()
    val toolState = inspectionToolState
    oldLevel = toolState.getLevel
    toolState.setLevel(forcedInspectionSeverity)
  }

  override def tearDown(): Unit = {
    inspectionToolState.setLevel(oldLevel)
    super.tearDown()
  }

  private def inspectionToolState: ScopeToolState = {
    val profile = ProjectInspectionProfileManager.getInstance(getFixture.getProject).getCurrentProfile
    profile.getToolDefaultState(inspectionEP.getShortName, getFixture.getProject)
  }

  private def inspectionEP =
    LocalInspectionEP.LOCAL_INSPECTION.getExtensions
      .find(_.implementationClass == classOfInspection.getCanonicalName)
      .get

  protected def forcedInspectionSeverity: HighlightDisplayLevel
}
