package cego.graphX

import cego.*

/**
 * https://graphviz.org/doc/info/colors.html
 * https://cloford.com/resources/charcodes/utf-8_mathematical.htm
 *
 * @param snapshot input
 * @param useName  false = generate byId; true byNAme
 */

class ToGraphviz(snapshot: Snap, useName: Boolean = false) extends ToGraph (snapshot) {

  private def mkLabelName(s: String): String = "\"" + s.replace("\"", "'") + "\""

  private def mkLabelName(n: CeNode): String =
    val str = snapshot.dict(n).replace("\"", "'")
    "\"" + str + symbols(n) + "\""

  def shorts(n: CeNode): String =
    if useName then mkLabelName(n)
    else ids(n)

  override def labelMapState(state: Boolean): String = if state then "" else "[label=\"~\" color=red]"

  override def revLabelMapState(state: Boolean): String = if state then "[label=\"#\"]" else "[label=\"~#\" color=red]"

  override def mkDrawingCommands: List[String] =
    val res = for (n <- snapshot.sequence) yield {
      n match {
        case _: NOT => Nil
        case x: Expr =>
          val dest = x.exprs.map(e => s"${shorts(target(e))} -> ${shorts(x)} ${label(e)}").toList
          s"${shorts(x)} [shape=ellipse ${mkColor(x)}]" :: dest
        case c: Constraint =>
          val dest = c.dests.map(e => s"${shorts(c)} -> ${shorts(target(e))} ${label(e)}").toList
          val spec = c match {
            case x: MASK => List(s"${shorts(c)} -> ${shorts(target(x.src))} ${revLabel(x.src)}")
            case x: REQ => List(s"${shorts(c)} -> ${shorts(target(x.src))} ${revLabel(x.src)}")
            case _ => Nil
          }
          s"${shorts(c)} [shape=ellipse ${mkColor(c)}] " :: spec ++ dest
        case x: NOTE =>
          val name = mkLabelName(x.text)
          val dest = x.targets.map {
            case r: Expr => s"$name -> ${shorts(r)} [color=azure4]"
            case r: Constraint => s"$name -> ${shorts(r)} [color=azure4]"
          }
          s"$name [ shape=rect ${mkColor(x)}]" :: dest.toList
      }
    }
    res.flatten

  override def mkAdocPic: String =
    s"""
       |["graphviz", "dia.png"]
       |----
       |digraph automata_0 {
       |rankdir=LR;
       |bgcolor=lightgray
       |
       |${mkDrawingCommands.mkString("\n")}
       |
       |}
       |----
       |""".stripMargin

  object graphvizColor extends NodeColor (
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
    mkColor(n, graphvizColor)

}
