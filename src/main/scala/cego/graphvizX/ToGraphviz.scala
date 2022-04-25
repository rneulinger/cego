package cego.graphvizX

import cego.*

import scala.::

/*
⊻	⊻	⊻	8891	22BB		XOR
⊼	⊼	⊼	8892	22BC		NAND
⊽	⊽	⊽	8893	22BD		NOR

∧	∧	∧	8743	2227	&and;	LOGICAL AND
∨	∨	∨	8744	2228	&or;	LOGICAL OR

✓	✓	✓	10003	2713		CHECK MARK
✔	✔	✔	10004	2714		HEAVY CHECK MARK
✕	✕	✕	10005	2715		MULTIPLICATION X
✖	✖	✖	10006	2716		HEAVY MULTIPLICATION X
✗	✗	✗	10007	2717		BALLOT X
✘	✘	✘	10008	2718		HEAVY BALLOT X

 */

/**
 * https://graphviz.org/doc/info/colors.html
 * https://cloford.com/resources/charcodes/utf-8_mathematical.htm
 *
 * @param snapshot input
 * @param useName  false = generate byId; true byNAme
 */

class ToGraphviz(snapshot: Snap, useName: Boolean = false) {
  private val notifier: Register = new Register
  private var ids: Map[CeNode, String] = Map()

  private val symbols = (for (x <- snapshot.sequence) yield {
    val sym =
      x match
        case _: CSE => "" // suc y

        case _: OR => " ∨"
        case _: AND => " ∧"
        case _: XOR => " ⊻"
        case _: NOR => " ⊽"
        case _: NAND => " ⊼"
        case _: NXOR => " ¬⊻"
        case _: EQ => " ="
        case _: NEQ => " ≠" // reverted simple

        case _: NOT => " ~"
        case _: NOTE => "" // txt

        case _: EXCL => " E"
        case _: INCL => " I"
        case _: ONE => " O" // unique
        case _: ANCHOR => " A" // fix
        case _: MASK => " M"
        case _: REQ => " R"
        case x: (Unary | Multi) => throw IllegalArgumentException(s"$x should never happen here") // make compiler happy
      end match
    x -> sym
  }).toMap

  def mkLabelName(s: String): String = "\"" + s.replace("\"", "'") + "\""

  def mkLabelName(n: CeNode): String =
    val str = snapshot.dict(n).replace("\"", "'")
    "\"" + str + symbols(n) + "\""

  def shorts(n: CeNode): String =
    if useName then mkLabelName(n)
    else ids(n)

  def target(e: Expr): Expr = e.logic()._1

  def label(e: Expr, inverted: Boolean = false): String =
    def mapBoolean(state: Boolean): String = if state then "" else "[label=\"~\" color=red]"

    inverted match {
      case false => mapBoolean(e.logic(inverted)._2)
      case true => mapBoolean(!e.logic(inverted)._2)
    }

  def revLabel(e: Expr, inverted: Boolean = false): String =
    def mapBoolean(state: Boolean): String = if state then "[label=\"#\"]" else "[label=\"~#\" color=red]"

    inverted match
      case false => mapBoolean(e.logic(inverted)._2)
      case true => mapBoolean(!e.logic(inverted)._2)

  def mkDrawingCommands: List[String] =
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
          val dest = x.targets.map(t =>
            t match {
              case r: Expr => s"$name -> ${shorts(r)} [color=azure4]"
              case r: Constraint => s"$name -> ${shorts(r)} [color=azure4]"
            }
          )
          s"$name [ shape=rect ${mkColor(x)}]" :: dest.toList
      }
    }
    res.flatten

  def mkAdocPic: String =
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

  def mkMessages: List[String] = Nil

  final def toAdoc: String = {

    val res = snapshot.sequence.filterNot(_.isInstanceOf[NOT])
    val legend =
      """
== Nodes

$nStr

== Logic
$gStr

"""

    val leg = mkLegend
    val nodes = leg.keySet.toList.sorted.map(x => s"* ${leg(x)} $x").mkString("\n")
    s""":doctyp: book
       |:toc: left
       |
       |== Overview
       |
       |$mkAdocPic
       |
       |== Nodes
       |
       |$nodes
       |""".stripMargin
  }

  def mkLegend: Map[String, String] = ids.filterNot(_._1.isInstanceOf[NOT]).map(c =>
    val truthy = c._1 match {
      case x: CSE => x.truthy
      case x: Expr => x.truthy
      case _ => ""
    }
    c._2 -> (snapshot.dict(c._1) + " : " + truthy)
  )

  snapshot.sequence.foreach { x =>
    val name =
      x match
        case _: CSE => mkName("c") // suc y

        case _: OR => mkName("or")
        case _: AND => mkName("and")
        case _: XOR => mkName("xor")
        case _: NOR => mkName("nor") // suc o
        case _: NAND => mkName("nand") // suc a
        case _: NXOR => mkName("nxor") // suc y
        case _: EQ => mkName("eq") // simple
        case _: NEQ => mkName("ne") // reverted simple

        case _: NOT => mkName("not")
        case _: NOTE => mkName("txt") // txt

        case _: EXCL => mkName("exc")
        case _: INCL => mkName("inc")
        case _: ONE => mkName("one") // unique
        case _: ANCHOR => mkName("anc") // fix
        case _: MASK => mkName("msk")
        case _: REQ => mkName("req")
        case _: (Unary | Multi) => throw IllegalArgumentException(s"$x should never happen here") // make compiler happy
      end match

    ids = ids + (x -> name)
  }

  /**
   * create a color for a node
   * @param x
   * @return color name for graphviz
   */
  def mkColor(x: CeNode): String =
    val pColor = "palegreen"
    val eColor = "lightpink"
    val nColor = "magenta"
    val cColor = "lightblue"
    val tColor = "antiquewhite"

    def mkColor(c: String): String =
      val t = c.trim
      if c.isEmpty then "" else s" color=$t style=filled"

    x match
      case _: CSE => mkColor(pColor) // suc y

      case _: OR => mkColor(eColor)
      case _: AND => mkColor(eColor)
      case _: XOR => mkColor(eColor)
      case _: NOR => mkColor(eColor) // suc o
      case _: NAND => mkColor(eColor) // suc a
      case _: NXOR => mkColor(eColor) // suc y
      case _: EQ => mkColor(eColor) // simple
      case _: NEQ => mkColor(eColor) // reverted simple

      case _: NOT => mkColor(nColor)
      case _: NOTE => mkColor(tColor) // txt

      case _: EXCL => mkColor(cColor)
      case _: INCL => mkColor(cColor)
      case _: ONE => mkColor(cColor) // unique
      case _: ANCHOR => mkColor(cColor) // fix
      case _: MASK => mkColor(cColor)
      case _: REQ => mkColor(cColor)
      case x: (Unary | Multi) => throw IllegalArgumentException(s"$x should never happen here") // make compiler happy
    end match

  private def mkName(s: String): String = s"$s${notifier.add(s)}"
}
