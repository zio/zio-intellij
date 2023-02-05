package zio.inspections

import zio.intellij.inspections.macros.{ErrorRendering, ProvideMacroZIO2Inspection}

class ProvideMacroZIO2InspectionTest extends ZScalaInspectionTest[ProvideMacroZIO2Inspection] {

  override protected def isZIO1 = false

  override protected def description = "There were issues while constructing a layer"

  override protected def descriptionMatches(s: String): Boolean = {
    val allPossibleErrors =
      Seq(
        "Please provide layers for the following",
        "Ambiguous layers!",
        "Circular Dependency Detected",
        "You have provided more arguments to provideSome than is required",
        ErrorRendering.unusedLayersWarning,
        ErrorRendering.superfluousProvideCustomWarning,
        ErrorRendering.provideSomeAnyEnvWarning
      )

    s != null && allPossibleErrors.exists(s.startsWith)
  }

  def testValidSimpleNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[String] = ???
       |${START}effect.provide(layer)$END""".stripMargin
  }.assertNotHighlighted()

  def testValidComplexNoHighlighting(): Unit = z {
    s"""
       |trait A
       |trait B
       |trait C
       |trait D
       |trait E
       |trait F
       |trait G
       |
       |val effect: URIO[A with D with G, Unit] = ???
       |
       |val a: ULayer[A] = ???
       |val b: URLayer[A, B] = ???
       |val c: URLayer[A with B, C] = ???
       |val d: URLayer[C with F, D] = ???
       |val e: ULayer[E] = ???
       |val f: URLayer[A with B with C with E, F] = ???
       |val g: URLayer[E with F, G] = ???
       |
       |${START}effect.provide(a, b, c, d, e, f, g)$END""".stripMargin
  }.assertNotHighlighted()

  def testSideEffectNoHighlighting(): Unit = z {
    s"""
       |val effect: UIO[Unit] = ???
       |val layer: ULayer[Unit] = ???
       |${START}effect.provide(layer)$END""".stripMargin
  }.assertNotHighlighted()

  def testDebugNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[String] = ???
       |${START}effect.provide(layer, ZLayer.Debug.tree)$END""".stripMargin
  }.assertNotHighlighted()

  def testSideEffectsWithServicesNoHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: ULayer[Unit with String] = ???
       |val sideEffect: ULayer[Unit] = ???
       |${START}effect.provide(layer, sideEffect)$END""".stripMargin
  }.assertNotHighlighted()

  def testCircularityToplevelHighlight(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer: URLayer[String, String] = ???
       |${START}effect.provide(layer)$END""".stripMargin
  }.assertHighlighted()

  def testCircularityTransitiveHighlight(): Unit = z {
    s"""
       |trait A
       |trait B
       |trait C
       |trait D
       |
       |val effect: URIO[D, Unit] = ???
       |
       |val a: URLayer[C, A] = ???
       |val b: URLayer[A, B] = ???
       |val c: URLayer[B, C] = ???
       |val d: URLayer[C, D] = ???
       |
       |${START}effect.provide(a, b, c, d)$END""".stripMargin
  }.assertHighlighted()

  def testMissingToplevelHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String with Int, Unit] = ???
       |val layer: ULayer[String] = ???
       |${START}effect.provide(layer)$END""".stripMargin
  }.assertHighlighted()

  def testMissingTransitiveHighlighting(): Unit = z {
    s"""
       |trait A
       |trait B
       |trait C
       |trait D
       |
       |val effect: URIO[D, Unit] = ???
       |
       |val b: URLayer[A, B] = ???
       |val c: URLayer[B, C] = ???
       |val d: URLayer[C, D] = ???
       |
       |${START}effect.provide(b, c, d)$END""".stripMargin
  }.assertHighlighted()

  def testDuplicateLayersDirectHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer1: ULayer[String] = ???
       |val layer2: ULayer[String] = ???
       |effect.provide(${START}layer1$END, ${START}layer2$END)""".stripMargin
  }.assertHighlighted()

  def testDuplicateLayersMixedHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String with Int, Unit] = ???
       |val layer1: URLayer[Boolean, Int with String] = ???
       |val layer2: URLayer[Char, Boolean] = ???
       |val layer3: ULayer[Char with String] = ???
       |effect.provide(${START}layer1$END, layer2, ${START}layer3$END)""".stripMargin
  }.assertHighlighted()

  def testUnusedHighlighting(): Unit = z {
    s"""
       |val effect: URIO[String, Unit] = ???
       |val layer1: ULayer[String] = ???
       |val layer2: ULayer[Boolean] = ???
       |effect.provide(layer1, ${START}layer2$END)""".stripMargin
  }.assertHighlighted()

}
