package zio.intellij.inspections.macros

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections.macros.LayerBuilder.{MermaidGraph, ZExpr}

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.collection.mutable

object MacroGraphUtils {

  def renderMermaid(element: ScExpression): Option[String] = {
    Option(element.getUserData(GraphDataKey)).map(renderMermaidLink)
  }

  private def renderMermaidLink(tree: LayerTree[ZExpr]) = {

    def escapeString(string: String): String =
      "\\\"" + string.replace("\"", "&quot") + "\\\""

    val map = tree
      .map(expr => escapeString(expr.value.getText))
      .fold[MermaidGraph](
        z = MermaidGraph.empty,
        value = MermaidGraph.make,
        composeH = _ ++ _,
        composeV = _ >>> _
      )
      .deps

    val aliases = mutable.Map.empty[String, String]

    def getAlias(name: String): String =
      aliases.getOrElse(
        name, {
          val alias = s"L${aliases.size}"
          aliases += name -> alias
          s"$alias($name)"
        }
      )

    val mermaidCode: String =
      map.flatMap {
        case (key, children) if children.isEmpty =>
          List(getAlias(key))
        case (key, children) =>
          children.map { child =>
            s"${getAlias(child)} --> ${getAlias(key)}"
          }
      }
        .mkString("\\n")

    val mermaidGraph =
      s"""{"code":"graph BT\\n$mermaidCode","mermaid": "{\\"theme\\": \\"default\\"}"}"""

    val encodedMermaidGraph: String =
      new String(Base64.getEncoder.encode(mermaidGraph.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8)

    val mermaidLink = s"https://mermaid.live/view/#$encodedMermaidGraph"
    mermaidLink
  }
}
