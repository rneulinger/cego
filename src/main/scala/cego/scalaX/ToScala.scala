package cego.scalaX

import cego.*

extension (p: Pred )
  def toCtor :String = p match {
    case x if x.muted => "Pred(" + p.truthy.toScalaString + ")"
    case x if x.negated => "Pred(" + p.truthy.toScalaString + ", Style.Negated)"
    case _ => "Pred(" + p.truthy.toScalaString + "," + p.falsy.toScalaString + ")"
  }

  def toArgs:String = p match{
    case x if x.muted => p.truthy.toScalaString
    case x if x.negated => p.truthy.toScalaString + ", Style.Negated"
    case _ => p.truthy.toScalaString + "," + p.falsy.toScalaString
  }


extension (s: Snap )
  def toScala:String = ToScala(s).toString

extension (g: Root )
  def toScala:String = ToScala(g.snap).toString

/**
 * convert to scala
 * @param snap
 */
final class ToScala(val snap:Snap):
  def xy: Map[CeNode, Pos] = snap.xy
  def dict: Map[CeNode, String] = snap.dict
  def sequence: Seq[CeNode] = snap.sequence
  val namesScala: Map[CeNode, String] = dict.map(x => (x._1, x._2.toScalaName))

  /**
   * create code for the whole graph.
   * @return
   */
  override def toString: String =
    def mkPos = {
      val res = xy.map(x =>
        val name = namesScala(x._1)
        s"setXY($name, ${x._2.x}, ${x._2.y})"
      )
      res.mkString("\n\t// xy -----\n\t", "\n\t", "\n")
    }

    val now = s"`${java.time.LocalTime.now()}`"
    val pre = s"import cego.*\nobject $now extends Root(${snap.title.toScalaString}){\n\t"
    sequence.map(mkScalaCode(_)).
      mkString(pre, "\n\t", "\n") +
      snap.effects().map(x => s"\t// ${dict(x)}").mkString("\t// ------------- effects\n", "\n", s"\n$mkPos\n}\nval g = $now\n")

  /**
   * create scala code for a given node
   * @param node
   * @return
   */
  private def mkScalaCode(node: CeNode): String =
    def neg(x: Expr | Constraint): String =
      x match {
        case e: Expr =>
          val (t, s) = e.logic()
          (if s then "" else "~") + namesScala(t)
        case c: Constraint => namesScala(c)
      }

    def n(n: Expr | Constraint): String = neg(n)

    def es(ns: Set[Expr]): String = ns.map(x => neg(x)).mkString(" ", ", ", " ")

    def es1(n: Expr, ns: Set[Expr]): String = (n :: ns.toList).map(x => neg(x)).mkString(" ", ", ", " ")

    def ns(ns: Set[Expr | Constraint]): String = ns.map(x => neg(x)).mkString(" ", ", ", " ")

    val name = namesScala(node)

    val decl = s"val $name ="
    val ps = es(node.exprs)

    def wrap(typ: String, args: String) =
      node match {
        case x: CSE => s":$typ = add( $typ($args), ${dict(node).toScalaString})"
        case x: EQ =>
          val p = Pred(x.truthy, x.falsy).toCtor
          s":$typ = add( $typ($args, $p), ${dict(node).toScalaString} )"
        case x: NEQ =>
          val p = Pred(x.truthy, x.falsy).toCtor
          s":$typ = add( $typ($args, $p), ${dict(node).toScalaString} )"
        case x: Expr =>
          val p = Pred(x.truthy, x.falsy).toCtor
          s":$typ = add( $typ($args)($p), ${dict(node).toScalaString} )"
        case x:NOTE => s":$typ = add( $typ($args), ${dict(node).toScalaString})"
        case _ => s":$typ = add( $typ($args), ${dict(node).toScalaString})"
      }
    val tmp = node.match {
      case CSE(p) if p.negated => wrap("CSE", s"${p.truthy.toScalaString}")
      case CSE(p) if p.full => wrap("CSE", s"${p.truthy.toScalaString},${p.falsy.toScalaString}")
      case CSE(_) => wrap("CSE", s"\"\" ")
      case NOTE(s, r) =>
        if( r.isEmpty) wrap("NOTE", s"${s.toScalaString}")
        else wrap("NOTE", s"${s.toScalaString}, ${ns(r)}")
      case NOT(e) => "" // s"NOT(${n(e)})"   // TODO Caution if this is an effect it will get lost
//      case EQ(e,p) => wrap("EQ", s"${n(e)}, ${p.toCtor}")
//      case NEQ(e,p) => wrap("NE", s"${n(e)}, ${p.toCtor}")
      case EQ(e,p) => wrap("EQ", s"${n(e)}")
      case NEQ(e,p) => wrap("NE", s"${n(e)}")
      case AND(e) => wrap("AND", s"${es(e)}")
      case OR(e) => wrap("OR", s"${es(e)}")
      case XOR(e) => wrap("XOR", s"${es(e)}")
      case NAND(e) => wrap("NAND", s"${es(e)}")
      case NOR(e) => wrap("NOR", s"${es(e)}")
      case NXOR(e) => wrap("NXOR", s"${es(e)}")
      case ANCHOR(e) => wrap("ANCHOR", s"${n(e)}")
      case ONE(xs) => wrap("ONE", s"${es(xs)}")
      case INCL(xs) => wrap("INCL", s"${es(xs)}")
      case EXCL(xs) => wrap("EXCL", s"${es(xs)}")
      case REQ(e, xs) => wrap("REQ", s"${es1(e, xs)}")
      case MASK(e, xs) => wrap("MASK", s"${es1(e, xs)}")
      case _ => throw IllegalArgumentException(s"unexpected node $n")
    }
    if tmp.isEmpty then ""
    else s"val $name$tmp"
