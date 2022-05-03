package cego.graphX

import cego.*

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

abstract class NodeColor(
                          val pColor: String,
                          val eColor: String,
                          val nColor: String,
                          val cColor: String,
                          val tColor: String
                        )

abstract class ToGraph (snapshot: Snap) {

  private val notifier: Register = new Register
  protected var ids: Map[CeNode, String] = Map()

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

  protected val symbols = (for (x <- snapshot.sequence) yield {
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

  def target(e: Expr): Expr = e.logic()._1

  def labelMapState(state: Boolean): String

  def label(e: Expr, inverted: Boolean = false): String =
    if (inverted) labelMapState(!e.logic(inverted)._2) 
    else labelMapState(e.logic(inverted)._2)

  def revLabelMapState(state: Boolean): String

  def revLabel(e: Expr, inverted: Boolean = false): String =
    if (inverted) revLabelMapState(!e.logic(inverted)._2) 
    else revLabelMapState(e.logic(inverted)._2)

  def mkDrawingCommands: List[String]

  def mkAdocPic: String

  def mkMessages: List[String] = Nil

  def mkLegend: Map[String, String] = ids.filterNot(_._1.isInstanceOf[NOT]).map(c =>
    val truthy = c._1 match {
      case x: CSE => x.truthy
      case x: Expr => x.truthy
      case _ => ""
    }
    c._2 -> (snapshot.dict(c._1) + " : " + truthy)
  )

  def mkColor(c: String): String

  def mkColor(x: CeNode): String

  def toAdoc: String = {

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

  private def mkName(s: String): String = s"$s${notifier.add(s)}"

  protected def mkColor(n: CeNode, nc: NodeColor): String =
    n match
      case _: CSE => mkColor(nc.eColor) // suc y

      case _: OR => mkColor(nc.eColor)
      case _: AND => mkColor(nc.eColor)
      case _: XOR => mkColor(nc.eColor)
      case _: NOR => mkColor(nc.eColor) // suc o
      case _: NAND => mkColor(nc.eColor) // suc a
      case _: NXOR => mkColor(nc.eColor) // suc y
      case _: EQ => mkColor(nc.eColor) // simple
      case _: NEQ => mkColor(nc.eColor) // reverted simple

      case _: NOT => mkColor(nc.nColor)
      case _: NOTE => mkColor(nc.tColor) // txt

      case _: EXCL => mkColor(nc.cColor)
      case _: INCL => mkColor(nc.cColor)
      case _: ONE => mkColor(nc.cColor) // unique
      case _: ANCHOR => mkColor(nc.cColor) // fix
      case _: MASK => mkColor(nc.cColor)
      case _: REQ => mkColor(nc.cColor)
      case x: (Unary | Multi) => throw IllegalArgumentException(s"$x should never happen here") // make compiler happy
    end match

}
