package zio.intellij.inspections

package object zioMethods {
  private[inspections] val `.*>`         = invocation("*>").from(zioClasses)
  private[inspections] val `.as`         = invocation("as").from(zioClasses)
  private[inspections] val `.map`        = invocation("map").from(zioClasses)
  private[inspections] val `.mapError`   = invocation("mapError").from(zioClasses)
  private[inspections] val `.orElseFail` = invocation("orElseFail").from(zioClasses)
  private[inspections] val `.catchAll`   = invocation("catchAll").from(zioClasses)
  private[inspections] val `.foldCause`  = invocation("foldCause").from(zioClasses)
  private[inspections] val `.foldCauseM` = invocation("foldCauseM").from(zioClasses)

  private[inspections] val `assert` = unqualified("assert").from(zioClasses)
}
