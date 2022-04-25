package cego.mermaidX

import cego.*

import scala.::


/**
 * https://mermaid-js.github.io/mermaid/#/flowchart
 * @param snapshot
 */
class ToMermaid(snapshot: Snap) {
  private val notifier: Register = new Register
  private var shorts: Map[CeNode, String] = Map()

  def target( e:Expr ):Expr = e.logic()._1

  def label( e:Expr, inverted: Boolean=false):String =
    //def mapBoolean( state:Boolean) :String = if state then "" else "[label=\"~\" color=red]"
    def mapBoolean( state:Boolean) :String = if state then "-->" else "-. NOT .->"
    inverted match{
      case false => mapBoolean(e.logic(inverted)._2)
      case true => mapBoolean(! e.logic(inverted)._2)
    }

  def cls( e:Expr, inverted: Boolean=false):String =
    //def mapBoolean( state:Boolean) :String = if state then "" else "[label=\"~\" color=red]"
    def mapBoolean( state:Boolean) :String = if state then "-->" else "-. NOT .->"
    inverted match{
      case false => mapBoolean(e.logic(inverted)._2)
      case true => mapBoolean(! e.logic(inverted)._2)
    }

  def revLabel(e:Expr, inverted: Boolean=false):String =
    //def mapBoolean( state:Boolean) :String = if state then "[label=\"#\"]" else "[label=\"~#\" color=red]"
    def mapBoolean( state:Boolean) :String = if state then "-->" else "-. NOT .->"
    inverted match{
      case false => mapBoolean(e.logic(inverted)._2)
      case true => mapBoolean(! e.logic(inverted)._2)
    }

  def mkDrawingCommands: List[String] =
    val res = for (n <- snapshot.sequence) yield {
      n match {
        case _: NOT => Nil
        case x: Expr =>
          x.exprs.map(e => s"  ${shorts(target(e))} ${label(e)} ${shorts(x)}").toList
          //s"${shorts(x)} [shape=ellipse ${mkColor(x)}]" :: dest
        case c: Constraint =>
          val dest = c.dests.map(e =>  s"  ${shorts(c)} ${label(e)} ${shorts(target(e))}").toList
          val spec = c match {
            //case x:ANCHOR => List(s"${shorts(c)} -> ${shorts(target(x.expr))} ${label(x.expr)}")
            case x:MASK => List(s"  ${shorts(c)} ${revLabel(x.src)} ${shorts(target(x.src))}")
            case x:REQ => List(s"  ${shorts(c)} ${revLabel(x.src)} ${shorts(target(x.src))}")
            case _ => Nil
          }
          spec ++ dest
          //s"${shorts(c)} [shape=ellipse]" :: spec ++ dest
        case x: NOTE => Nil
      }
    }
    res.flatten

  def mkAdocPic:String =

    s"""
       |[mermaid, "sample", png]
       |....
       |%%{init: {'theme': 'base', 'themeVariables': { 'primaryColor': 'palegreen'}}}%%
       |flowchart LR
       |${mkDrawingCommands.mkString("\n")}
       |
       |  classDef red fill:red;
       |  classDef green fill:palegreen;
       |....
       |       |""".stripMargin

  def mkMessages:List[String] = Nil

  final def toAdoc = {

    val res = snapshot.sequence.filterNot(_.isInstanceOf[NOT])
    //val sorted = snapshot.causes.toList.sortBy(_.num)
    //val nStr = sorted.map(_.legend).flatten.mkString("\n")
    //val gates = members.map(_._1).toList.filterNot( _.isInstanceOf[C|Alias|NOT])
    //val gStr = gates.map(_.legend).flatten.mkString("\n")
    val legend ="""
== Nodes

$nStr

== Logic
$gStr

"""
      val pre =
        """
          |["graphviz", "dia.png"]
          |----
          |digraph automata_0 {
          |rankdir=LR;
          |bgcolor=lightgray
          |
          |""".stripMargin
      val post =
        """
          |}
          |----
          |""".stripMargin

    val leg = mkLegend
    leg.keySet.toList.sorted.map( x => s"* ${leg(x)} $x").mkString("\n")
  }

  def mkLegend: Map[String, String] = shorts.filterNot(_._1.isInstanceOf[NOT] ).map(c =>
    val truthy = c._1 match {
      case x: CSE => x.truthy
      case x: Expr => x.truthy
      case _ => ""
    }
    (c._2 -> ( snapshot.dict(c._1) + " : " + truthy))
  )

  snapshot.sequence.foreach { x =>
    val name = x match {
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
      case _:(Unary|Multi) => throw IllegalArgumentException( s"$x should never happen here") // make compiler happy

    }
    shorts = shorts + (x -> name)
  }


  def mkColor(x:CeNode) =
    def mkColor(c:String):String=
      val t=c.trim
      if c.isEmpty then "" else s" color=$t style=filled"

    x match {
      case _: CSE => mkColor("palegreen") // suc y

      case _: OR => mkColor("lightpink")
      case _: AND => mkColor("lightpink")
      case _: XOR => mkColor("lightpink")
      case _: NOR => mkColor("lightpink") // suc o
      case _: NAND => mkColor("lightpink") // suc a
      case _: NXOR => mkColor("lightpink") // suc y
      case _: EQ => mkColor("lightpink") // simple
      case _: NEQ => mkColor("lightpink") // reverted simple

      case _: NOT => mkColor("magenta")
      case _: NOTE => mkColor("navy") // txt

      case _: EXCL => mkColor("lightblue")
      case _: INCL => mkColor("lightblue")
      case _: ONE => mkColor("lightblue") // unique
      case _: ANCHOR => mkColor("lightblue") // fix
      case _: MASK => mkColor("lightblue")
      case _: REQ => mkColor("lightblue")
      case x:(Unary|Multi) => throw IllegalArgumentException( s"$x should never happen here") // make compiler happy
    }

  private def mkName(s: String): String = s"$s${notifier.add(s)}"
}
