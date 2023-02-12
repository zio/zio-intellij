package zio.inspections

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import zio.intellij.inspections.macros.{ErrorRendering, ProvideMacroInspection}

abstract class ProvideMacroInspectionTestBase extends ZScalaInspectionTest[ProvideMacroInspection] {

  override protected def description = "There were issues while constructing a layer"

  protected def allPossibleErrors: List[String] =
    List(
      "Please provide layers for the following",
      "Ambiguous layers!",
      "Circular Dependency Detected",
      "You have provided more arguments to provideSome than is required",
      ErrorRendering.unusedLayersWarning,
      ErrorRendering.superfluousProvideCustomWarning,
      ErrorRendering.provideSomeAnyEnvWarning
    )

  override protected def descriptionMatches(s: String): Boolean =
    s != null && allPossibleErrors.exists(s.startsWith)

  protected def r(str: String): String = s"$START$str$END"

}

abstract class ProvideMacroInspectionTest(val provide: String, val Has: String => String, val imports: String)
    extends ProvideMacroInspectionTestBase {

  def testValidSimpleNoHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")}, Unit] = ???
       |val layer: ULayer[${Has("String")}] = ???
       |effect.$provide(${r("layer")})""".stripMargin
  }.assertNotHighlighted()

  def testValidComplexNoHighlighting(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |trait E
       |trait F
       |trait G
       |
       |val effect: URIO[${Has("A")} with ${Has("D")} with ${Has("G")}, Unit] = ???
       |
       |val a: ULayer[${Has("A")}] = ???
       |val b: URLayer[${Has("A")}, ${Has("B")}] = ???
       |val c: URLayer[${Has("A")} with ${Has("B")}, ${Has("C")}] = ???
       |val d: URLayer[${Has("C")} with ${Has("F")}, ${Has("D")}] = ???
       |val e: ULayer[${Has("E")}] = ???
       |val f: URLayer[${Has("A")} with ${Has("B")} with ${Has("C")} with ${Has("E")}, ${Has("F")}] = ???
       |val g: URLayer[${Has("E")} with ${Has("F")}, ${Has("G")}] = ???
       |
       |${r(s"effect.$provide(a, b, c, d, e, f, g)")}""".stripMargin
  }.assertNotHighlighted()

  def testCircularityTopLevelHighlight(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")}, Unit] = ???
       |val layer: URLayer[${Has("String")}, String] = ???
       |${r(s"effect.$provide(layer)")}""".stripMargin
  }.assertHighlighted()

  def testCircularityTransitiveHighlight(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |
       |val effect: URIO[${Has("D")}, Unit] = ???
       |
       |val a: URLayer[${Has("C")}, ${Has("A")}] = ???
       |val b: URLayer[${Has("A")}, ${Has("B")}] = ???
       |val c: URLayer[${Has("B")}, ${Has("C")}] = ???
       |val d: URLayer[${Has("C")}, ${Has("D")}] = ???
       |
       |${r(s"effect.$provide(a, b, c, d)")}""".stripMargin
  }.assertHighlighted()

  def testMissingTopLevelHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")}, Unit] = ???
       |val layer: ULayer[${Has("String")}] = ???
       |${r(s"effect.$provide(layer)")}""".stripMargin
  }.assertHighlighted()

  def testMissingTransitiveHighlighting(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |
       |val effect: URIO[${Has("D")}, Unit] = ???
       |
       |val b: URLayer[${Has("A")}, ${Has("B")}] = ???
       |val c: URLayer[${Has("B")}, ${Has("C")}] = ???
       |val d: URLayer[${Has("C")}, ${Has("D")}] = ???
       |
       |${r(s"effect.$provide(b, c, d)")}""".stripMargin
  }.assertHighlighted()

  def testDuplicateLayersDirectHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")}, Unit] = ???
       |val layer1: ULayer[${Has("String")}] = ???
       |val layer2: ULayer[${Has("String")}] = ???
       |effect.$provide(${r("layer1")}, ${r("layer2")})""".stripMargin
  }.assertHighlighted()

  def testDuplicateLayersMixedHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")}, Unit] = ???
       |val layer1: URLayer[${Has("Boolean")}, ${Has("Int")} with ${Has("String")}] = ???
       |val layer2: URLayer[${Has("Char")}, ${Has("Boolean")}] = ???
       |val layer3: ULayer[${Has("Char")} with ${Has("String")}] = ???
       |effect.$provide(${r("layer1")}, layer2, ${r("layer3")})""".stripMargin
  }.assertHighlighted()

  def testUnusedHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")}, Unit] = ???
       |val layer1: ULayer[${Has("String")}] = ???
       |val layer2: ULayer[${Has("Boolean")}] = ???
       |effect.$provide(layer1, ${r("layer2")})""".stripMargin
  }.assertHighlighted()

}

class ProvideMacroZIO1InspectionTest
    extends ProvideMacroInspectionTest("inject", tpe => s"Has[$tpe]", "import zio.magic._") {

  override protected def librariesLoaders: Seq[LibraryLoader] =
    IvyManagedLoader("io.github.kitlangton" %% "zio-magic" % "0.3.12") +: super.librariesLoaders

  override protected def allPossibleErrors: List[String] = "Contains non-Has types" +: super.allPossibleErrors

  override protected def isZIO1 = true

  def testTopLevelNonHasHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[String] = ???
       |${r(s"effect.$provide(layer)")}""".stripMargin
  }.assertHighlighted()

  def testTransitiveNonHasHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")}, Unit] = ???
       |val layer1: ULayer[Int] = ???
       |val layer2: URLayer[Int, ${Has("String")}] = ???
       |${r(s"effect.$provide(layer1, layer2)")}""".stripMargin
  }.assertHighlighted()

}

class ProvideMacroZIO2InspectionTest extends ProvideMacroInspectionTest("provide", identity, "") {

  override protected def isZIO1 = false

  def testSideEffectNoHighlighting(): Unit = z {
    s"""
       |val effect: UIO[Unit] = ???
       |val layer: ULayer[Unit] = ???
       |effect.provide(${r("layer")})""".stripMargin
  }.assertNotHighlighted()

  def testDebugNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[String] = ???
       |effect.provide(layer, ${r("ZLayer.Debug.tree")})""".stripMargin
  }.assertNotHighlighted()

  def testSideEffectsWithServicesNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[Unit with String] = ???
       |val sideEffect: ULayer[Unit] = ???
       |effect.provide(layer, ${r("sideEffect")})""".stripMargin
  }.assertNotHighlighted()

}

abstract class ProvideSomeMacroInspectionTest(val provideSome: String, val Has: String => String, val imports: String)
    extends ProvideMacroInspectionTestBase {

  def testValidSimpleNoHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")}, Unit] = ???
       |val layer: ULayer[${Has("String")}] = ???
       |effect.$provideSome[${Has("Int")}](${r("layer")})""".stripMargin
  }.assertNotHighlighted()

  def testValidComplexNoHighlighting(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |trait E
       |trait F
       |trait G
       |
       |val effect: URIO[${Has("A")} with ${Has("D")} with ${Has("G")}, Unit] = ???
       |
       |val a: ULayer[${Has("A")}] = ???
       |val b: URLayer[${Has("A")}, ${Has("B")}] = ???
       |val c: URLayer[${Has("A")} with ${Has("B")}, ${Has("C")}] = ???
       |val d: URLayer[${Has("C")} with ${Has("F")}, ${Has("D")}] = ???
       |val f: URLayer[${Has("A")} with ${Has("B")} with ${Has("C")} with ${Has("E")}, ${Has("F")}] = ???
       |val g: URLayer[${Has("E")} with ${Has("F")}, ${Has("G")}] = ???
       |
       |${r(s"effect.$provideSome[${Has("E")}](a, b, c, d, f, g)")}""".stripMargin
  }.assertNotHighlighted()

  def testCircularityTopLevelHighlight(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")}, Unit] = ???
       |val layer: URLayer[${Has("String")}, String] = ???
       |${r(s"effect.$provideSome[${Has("Int")}](layer)")}""".stripMargin
  }.assertHighlighted()

  def testCircularityTransitiveHighlight(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |
       |val effect: URIO[${Has("D")} with ${Has("A")}, Unit] = ???
       |
       |val a: URLayer[${Has("C")}, ${Has("A")}] = ???
       |val b: URLayer[${Has("A")}, ${Has("B")}] = ???
       |val c: URLayer[${Has("B")}, ${Has("C")}] = ???
       |val d: URLayer[${Has("C")}, ${Has("D")}] = ???
       |
       |${r(s"effect.$provideSome[${Has("A")}](a, b, c, d)")}""".stripMargin
  }.assertHighlighted()

  def testMissingTopLevelHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")} with ${Has("Boolean")}, Unit] = ???
       |val layer: ULayer[${Has("String")}] = ???
       |${r(s"effect.$provideSome[${Has("Boolean")}](layer)")}""".stripMargin
  }.assertHighlighted()

  def testMissingTransitiveHighlighting(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |
       |val effect: URIO[${Has("D")} with ${Has("Int")}, Unit] = ???
       |
       |val b: URLayer[${Has("A")}, ${Has("B")}] = ???
       |val c: URLayer[${Has("B")}, ${Has("C")}] = ???
       |val d: URLayer[${Has("C")}, ${Has("D")}] = ???
       |
       |${r(s"effect.$provideSome[${Has("Int")}](b, c, d)")}""".stripMargin
  }.assertHighlighted()

  def testDuplicateLayersDirectHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")}, Unit] = ???
       |val layer1: ULayer[${Has("String")}] = ???
       |val layer2: ULayer[${Has("String")}] = ???
       |effect.$provideSome[${Has("Int")}](${r("layer1")}, ${r("layer2")})""".stripMargin
  }.assertHighlighted()

  def testDuplicateLayersMixedHighlighting(): Unit = z {
    s"""$imports
       |
       |trait A
       |trait B
       |trait C
       |trait D
       |trait E
       |
       |val effect: URIO[${Has("A")} with ${Has("B")} with ${Has("E")}, Unit] = ???
       |val layer1: URLayer[${Has("C")}, ${Has("B")} with ${Has("A")}] = ???
       |val layer2: URLayer[${Has("D")}, ${Has("C")}] = ???
       |val layer3: ULayer[${Has("D")} with ${Has("A")}] = ???
       |effect.$provideSome[${Has("B")}](${r("layer1")}, layer2, ${r("layer3")})""".stripMargin
  }.assertHighlighted()

  def testUnusedHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Int")}, Unit] = ???
       |val layer1: ULayer[${Has("String")}] = ???
       |val layer2: ULayer[${Has("Boolean")}] = ???
       |effect.$provideSome[${Has("Int")}](layer1, ${r("layer2")})""".stripMargin
  }.assertHighlighted()

}

class ProvideSomeMacroZIO1InspectionTest
    extends ProvideSomeMacroInspectionTest("injectSome", tpe => s"Has[$tpe]", "import zio.magic._") {

  override protected def librariesLoaders: Seq[LibraryLoader] =
    IvyManagedLoader("io.github.kitlangton" %% "zio-magic" % "0.3.12") +: super.librariesLoaders

  override protected def allPossibleErrors: List[String] = "Contains non-Has types" +: super.allPossibleErrors

  override protected def isZIO1 = true

  def testTopLevelNonHasHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[String with Int, Unit] = ???
       |val layer: ULayer[String with Int] = ???
       |${r(s"effect.$provideSome(layer)")}""".stripMargin
  }.assertHighlighted()

  def testTransitiveNonHasHighlighting(): Unit = z {
    s"""$imports
       |
       |val effect: URIO[${Has("String")} with ${Has("Boolean")}, Unit] = ???
       |val layer1: ULayer[Int] = ???
       |val layer2: URLayer[Int, ${Has("String")}] = ???
       |${r(s"effect.$provideSome[${Has("Boolean")}](layer1, layer2)")}""".stripMargin
  }.assertHighlighted()

}

class ProvideSomeMacroZIO2InspectionTest extends ProvideSomeMacroInspectionTest("provideSome", identity, "") {

  override protected def isZIO1 = false

  def testSideEffectNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[Unit] = ???
       |effect.provideSome[String](${r("layer")})""".stripMargin
  }.assertNotHighlighted()

  def testDebugNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String with Int, Unit] = ???
       |val layer: ULayer[String] = ???
       |effect.provideSome[Int](layer, ${r("ZLayer.Debug.tree")})""".stripMargin
  }.assertNotHighlighted()

  def testSideEffectsWithServicesNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String with Int, Unit] = ???
       |val layer: ULayer[Unit with String] = ???
       |val sideEffect: ULayer[Unit] = ???
       |effect.provideSome[Int](layer, ${r("sideEffect")})""".stripMargin
  }.assertNotHighlighted()

  def testAnyEnvHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[String] = ???
       |${r("effect.provideSome[Any](layer)")}""".stripMargin
  }.assertHighlighted()

}
