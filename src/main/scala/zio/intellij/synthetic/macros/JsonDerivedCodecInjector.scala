package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

/**
 * Similar to [[org.jetbrains.plugins.scala.externalLibraries.circe.CirceCodecInjector]].
 * Does not do typechecking etc and generates only a codec, so users may face errors when running actual compilation.
 */
class JsonDerivedCodecInjector extends SyntheticMembersInjector {
  private val jsonDerive = "_root_.zio.json.jsonDerive"
  private val JsonCodec  = "_root_.zio.json.JsonCodec"
  private val `???`      = "_root_.scala.Predef.???"

  // fast check
  private def hasCodecs(source: ScTypeDefinition): Boolean =
    source.findAnnotationNoAliases(jsonDerive) != null

  // so annotated sealed traits will generate a companion
  override def needsCompanionObject(source: ScTypeDefinition): Boolean = hasCodecs(source)

  // add implicits to sealed trait / case class companions
  override def injectFunctions(source: ScTypeDefinition): Seq[String] =
    source match {
      case obj: ScObject =>
        obj.fakeCompanionClassOrCompanionClass match {
          case clazz: ScTypeDefinition if hasCodecs(clazz) => genImplicits(clazz)
          case _                                           => Nil
        }
      case _ => Nil
    }

  private def genImplicits(clazz: ScTypeDefinition): Seq[String] = {
    val typeParameters = clazz.typeParameters.map(_.name)
    val (codecParams, codecCtxBounds) =
      if (typeParameters.isEmpty)
        ("", "")
      else
        (s"[${join(typeParameters)}]", s"[${join(typeParameters.map(p => s"$p: $JsonCodec"))}]")

    val name = clazz.name
    Seq(s"implicit def codecFor$name$codecCtxBounds: $JsonCodec[$name$codecParams] = ${`???`}")
  }

  private def join(xs: Seq[String]): String = xs.mkString(",")

}
