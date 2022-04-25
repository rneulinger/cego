package cego.erpelX

import cego.*

/**
 * create expressions for the usage in Erpelstolz Logikrechner.
 * https://www.erpelstolz.at/gateway/formular-zentral.html
 *
 * @param snap snapshot from ceg
 */
case class ToErpel(snap: Snap) {
  def this(root: Root) = this(root.snap)

  /**
   * maps causes to name in Erplestolz
   */
  val nodes: Map[CSE, String] = snap.sequence.collect { case t: CSE => t }.
    zipWithIndex.map(x => x._1 -> ToErpel.num(x._2)).toMap
  /**
   *
   */
  val exprs: Map[String, String] = snap.sequence.collect { case t: Expr => t }.
    map(x => snap.short(x) -> expr(x)).toMap

  def name(x: CSE): String = nodes(x)

  def expr(x: Expr): String = x match {
    case x: CSE => name(x)
    case x: NOT => ToErpel.Negate + expr(x.expr)
    case x: EQ => expr(x.expr)
    case x: AND => x.exprs match {
      case x if x.isEmpty => ToErpel.Verum
      case x if x.size == 1 => expr(x.head)
      case x => x.map(e => expr(e)).mkString("(", " & ", ")")
    }
    case x: OR => x.exprs match {
      case x if x.isEmpty => ToErpel.Falsum
      case x if x.size == 1 => expr(x.head)
      case x => x.map(e => expr(e)).mkString("(", " | ", ")")
    }
    case x: NAND => x.exprs match {
      case x if x.isEmpty => ToErpel.Falsum
      case x if x.size == 1 => ToErpel.Negate + expr(x.head)
      case x => x.map(e => expr(e)).mkString("~(", " & ", ")")
    }
    case x: NOR => x.exprs match {
      case x if x.isEmpty => ToErpel.Verum
      case x if x.size == 1 => ToErpel.Negate + expr(x.head)
      case x => x.map(e => expr(e)).mkString("~(", " | ", ")")
    }
    case x: XOR => "ERROR: XOR not implemented yet" // TODO implement
    case x: NXOR => "ERROR: NXOR not implemented yet" //  TODO implement
    case x: (Unary | Multi) => throw IllegalArgumentException(s"$x should never happen here") // make compiler happy
  }
}

object ToErpel:
  /**
   * legal characters to start a node in erpelstolz
   */
  //noinspection SpellCheckingInspection
  val LegalFirstCharsForNames = "abcedeghijklmnopqrsuvwxyz"
  /**
   * False in Erpelstolz
   */
  val Falsum = "0"
  /**
   * True in Erpelstolz
   */
  val Verum = "1"

  /**
   * Not in Erpelstolz
   */
  val Negate = "~"

  /**
   * create a name for a variable in Erpelstolz
   * @param i number of node >= 0
   * @return name of variable to be used in Erpelstolz formulas
   */
  def num(i: Int): String =
    require( i >= 0, "negative number")
    val hi = i / LegalFirstCharsForNames.length
    val lo = i % LegalFirstCharsForNames.length
    if hi == 0 then LegalFirstCharsForNames.substring(i, i + 1)
    else LegalFirstCharsForNames.substring(lo, lo + 1) + hi.toString

extension (ceg: Root)

/**
 * returns formula for a given expression.
 */
  inline def toErpel(e: Expr): String =
    if !ceg.contains(e) then s"???$e"
    else ToErpel(ceg.snap).expr(e)

  inline def mkErpel: ToErpel = ToErpel(ceg.snap)
