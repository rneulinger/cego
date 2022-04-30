package cego.graphX

import cego.*
import cego.graphX.ToGraph

import scala.::


/**
 * https://mermaid-js.github.io/mermaid/#/flowchart
 *
 * @param snapshot snapshot to be processed
 */
class ToMermaid(snapshot: Snap) extends ToGraph (snapshot) {

  override def labelMapState(state: Boolean): String = if state then "-->" else "-. NOT .->"

  override def revLabelMapState(state: Boolean): String = if state then "-->" else "-. NOT .->"

  override def mkDrawingCommands: List[String] =
    val res = for (n <- snapshot.sequence) yield {
      n match {
        case _: NOT => Nil
        case x: Expr =>
          x.exprs.map(e => s"  ${ids(target(e))} ${label(e)} ${ids(x)}").toList
        //s"${ids(x)} [shape=ellipse ${mkColor(x)}]" :: dest
        case c: Constraint =>
          val dest = c.dests.map(e => s"  ${ids(c)} ${label(e)} ${ids(target(e))}").toList
          val spec = c match {
            //case x:ANCHOR => List(s"${ids(c)} -> ${ids(target(x.expr))} ${label(x.expr)}")
            case x: MASK => List(s"  ${ids(c)} ${revLabel(x.src)} ${ids(target(x.src))}")
            case x: REQ => List(s"  ${ids(c)} ${revLabel(x.src)} ${ids(target(x.src))}")
            case _ => Nil
          }
          spec ++ dest
        //s"${ids(c)} [shape=ellipse]" :: spec ++ dest
        case _: NOTE => Nil
      }
    }
    res.flatten

  override def mkAdocPic: String =
    s"""
       |[mermaid, "sample", png]
       |----
       |%%{init: {'theme': 'base', 'themeVariables': { 'primaryColor': 'palegreen'}}}%%
       |flowchart LR
       |${mkDrawingCommands.mkString("\n")}
       |
       |  classDef red fill:red;
       |  classDef green fill:palegreen;
       |----
       |""".stripMargin

  object mermaidColor extends NodeColor (
    pColor = "palegreen",
    eColor = "lightpink",
    nColor = "magenta",
    cColor = "lightblue",
    tColor = "antiquewhite"
  )

  def mkColor(c: String): String =
    val t = c.trim
    if c.isEmpty then "" else s" color=$t style=filled"

  def mkColor(n: CeNode): String =
    mkColor(n, mermaidColor)
}
