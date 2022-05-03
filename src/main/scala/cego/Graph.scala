package cego

import scala.annotation.tailrec

case class Inf(name: String, pred: Pred):
  def truthy: String = pred.truthy

  def falsy: String = pred.falsy

object Inf:
  def apply(name: String, truthy: String, falsy: String): Inf = new Inf(name, Pred(truthy, falsy))

  def apply(name: String = "", truthy: String = "", style: Style = Muted): Inf = new Inf(name, Pred(truthy, style))

type Desc = String|(String, String)|Inf

def toInf(x:Desc):Inf = x match{
  case x: String => Inf(x,x)
  case x: (String,String) => Inf(x._1, x._2)
  case x: Inf => x
}

val Ano = Inf()

def valid(short: String, full: String): Inf = Inf(s"$short valid", s"\"$full\" = 1 (valid)", s"\"$full\" = 0 (invalid)")
def aktiv(short: String, full: String): Inf = Inf(s"$short aktiv", s"\"$full\" = 1 (active)", s"\"$full\" = 0 (inactive)")

/**
 *
 */
given Conversion[Inf, Pred] = _.pred

enum Obs extends Enum[Obs] {
  case Forced
  case Observable
  case Normal
}

val Forced = Obs.Forced
val Observable = Obs.Observable
val Normal = Obs.Normal

/**
 * Cause Effect Graph
 */
trait CEG extends MixIn :
  /**
   * return Root for this graph
   *
   * @return
   */
  def root: Root

  /**
   * set position for a given element
   *
   * @param node to set position
   * @param x    position
   * @param y    position
   */
  def setXY(node: CeNode, x: Int, y: Int): Unit

  /**
   * set observability for a given element.
   *
   * @param expr expression
   * @param obs  kind of observability
   */
  def setObs(expr: Expr, obs: Obs = Obs.Observable): Unit

  /**
   * set UI type
   *
   * @param expr expression
   * @param ui   name of UI type
   */
  def setUi(expr: Expr, ui: String): Unit

  /**
   * add a node to this graph
   *
   * @param x    node
   * @param name of node within this graph
   * @tparam T type of Node
   * @return added node
   */
  def add[T <: CeNode](x: T, name: String = ""): T

  /**
   * add a node
   * @param node
   * @param name
   * @return
   */
  def addX(node: CeNode, name: String): CeNode

  /**
   * set position for a given element grid-based.
   *
   * @param node to set position for
   * @param x    horizontal position in grid
   * @param y    vertical position in grid
   */
  final def grid(node: CeNode, x: Int, y: Int): Unit = setXY(node, (x * 200) + 20, y * 20 + 20)

  /**
   * set position for a given element grid-based.
   * hoizontal position is determined by rank of the node
   *
   * @param node
   * @param y
   */
  final def grid(node: CeNode, y: Int): Unit = setXY(node, ((node.rank - 1) * 200) + 20, y * 20 + 20)

  /**
   * create a set of causes constrained by ONE.
   *
   * @param e1   1st expression
   * @param e2   2nd expression
   * @param more additional expressions
   * @return
   */
  final def ones(e1: Desc, e2: Desc, more: Desc*): List[CSE] =
    val all = Seq(e1, e2) ++ more
    val names = atLeast(2, all)
    assert(all.size == names.size)
    val causes = all.map(toInf).map(x => cse(x)).toList
    println(causes)
    add(ONE(causes *))
    causes

  /**
   * create causes constrained by ONE.
   *
   * @param es name of nodes
   * @return
   */
  final def ones(es: Seq[String]): Seq[CSE] =
    require(es.size >= 2, s"size must be >=2; found ${es.size}")
    ones(es.head, es.tail.head, es.drop(2) *)

  final def ones(es: Map[String, String]): Map[String, CSE] =
    require(es.size >= 2, s"size must be >=2; found ${es.size}")
    val a :: b :: more = es.map(x => (x._1 -> x._2)).toSeq
    val cses = ones(a, b, more *)
    es.keySet.zip(cses).toMap


  /**
   * create a set of causes constrained by EXCL.
   *
   * @param e1   1st expression
   * @param e2   2nd expression
   * @param more additional expressions
   * @return
   */
  final def excls(e1: Desc, e2: Desc, more: Desc*): List[CSE] =
    val all = Seq(e1, e2) ++ more
    val names = atLeast(2, all)
    require(all.size == names.size)
    val causes = all.map(toInf).map(x => cse(x)).toList
    add(EXCL(causes *))
    causes

  final def excls(es: Seq[String]): Seq[CSE] =
    require(es.size >= 2)
    excls(es.head, es.tail.head, es.drop(2) *)

  final def excls(es: Map[String, String]): Map[String, CSE] =
    require(es.size >= 2, s"size must be >=2; found ${es.size}")
    val a :: b :: more = es.map(x => (x._1 -> x._2)).toSeq
    val cses = excls(a, b, more *)
    es.keySet.zip(cses).toMap

  /**
   * create a list of causes constrained by INCL.
   *
   * @param e1   1st expression
   * @param e2   2nd expression
   * @param more additional expressions
   * @return created causes.
   */
  final def incls(e1: Desc, e2: Desc, more: Desc*): List[CSE] =
    val all = Seq(e1, e2) ++ more
    val names = atLeast(2, all)
    require(all.size == names.size)
    val causes = all.map(toInf).map(x => cse(x)).toList
    add(INCL(causes *))
    causes


  final def incls(es: Seq[String]): Seq[CSE] =
    require(es.size >= 2)
    incls(es.head, es.tail.head, es.drop(2) *)

  final def incls(es: Map[String, String]): Map[String, CSE] =
    require(es.size >= 2, s"size must be >=2; found ${es.size}")
    val a :: b :: more = es.map(x => (x._1 -> x._2)).toSeq
    val cses = incls(a, b, more *)
    es.keySet.zip(cses).toMap

  /**
   * create a cause
   *
   * @param inf
   * @return
   */
  final def cse(inf: Inf): CSE =
    require(inf.name.nonEmpty)
    val res = if inf.truthy.isEmpty then CSE(inf.name, inf.falsy) else CSE(inf)
    add(inf.name, res)
    res

  final def cse(name: String, truthy: String, falsy: String): CSE = cse(Inf(name, truthy, falsy))

  final def cse(name: String, truthy: String, style: Style = Muted): CSE = cse(Inf(name, truthy, style))

  /**
   * EQ from Inf
   * @param e  input
   * @param inf name and predicate
   * @return created and registered EQ node
   */
  final def eq(e: Expr, inf: Inf): EQ =
    val expr = EQ(e, inf)
    add(inf.name, expr)
    expr

  /**
   * EQ from truthy/falsy
   * @param e input
   * @param name of node
   * @param truthy full description of true state
   * @param falsy full description of false state
   * @return created and registered EQ node
   */
  final def eq(e: Expr)(name: String, truthy: String, falsy: String): EQ = eq(e, Inf(name, truthy, falsy))

  /**
   * EQ without falsy
   * @param e input
   * @param name
   * @param truthy
   * @param style
   * @return created and registered EQ node
   */
  final def eq(e: Expr)(name: String, truthy: String, style: Style = Muted): EQ = eq(e, Inf(name, truthy, style))

  /**
   * EQ as an intermediate node.
   * @param e input
   */
  final def eq(e: Expr): EQ = eq(e, Ano)

  /**
   * AND from Inf
   *
   * @param es  inputs
   * @param inf name and predicate
   * @return
   */
  final def and(es: Set[Expr], inf: Inf): AND =
    val res = AND(es)(inf)
    add(inf.name, res)
    res

  final def and(e1: Expr, e2: Expr, more: Expr*)(inf: Inf): AND = and(unique2(e1, e2, more *), inf)

  final def and(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, falsy: String): AND =
    and(unique2(e1, e2, more *), Inf(name, truthy, falsy))

  final def and(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, style: Style = Muted): AND =
    and(unique2(e1, e2, more *), Inf(name, truthy, style))

  final def and(e1: Expr, e2: Expr, more: Expr*)(name: String): AND =
    and(unique2(e1, e2, more *), Inf(name, "", Muted))

  final def and(e1: Expr, e2: Expr, more: Expr*): AND = and(e1, e2, more *)(Ano)

  final def and(n: Int): AND =
    require(n >= 2)
    val causes = (1 to n).map(x => cse(Inf(x.toString))).toList
    and(causes.head, causes(1), causes.drop(2) *)

  /**
   * OR Nodes
   *
   * @param es  inputs
   * @param inf name and predicate
   * @return
   */
  final def or(es: Set[Expr], inf: Inf): OR =
    val res = OR(es)(inf)
    add(inf.name, res)
    res

  final def or(e1: Expr, e2: Expr, more: Expr*)(inf: Inf): OR = or(unique2(e1, e2, more *), inf)

  final def or(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, falsy: String): OR =
    or(unique2(e1, e2, more *), Inf(name, truthy, falsy))

  final def or(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, style: Style = Muted): OR =
    or(unique2(e1, e2, more *), Inf(name, truthy, style))

  final def or(e1: Expr, e2: Expr, more: Expr*)(name: String): OR =
    or(unique2(e1, e2, more *), Inf(name, "", Muted))

  final def or(e1: Expr, e2: Expr, more: Expr*): OR = or(e1, e2, more *)(Ano)

  /**
   * XOR Nodes
   *
   * @param es  inputs
   * @param inf name and predicate
   * @return
   */

  final def xor(es: Set[Expr], inf: Inf): XOR =
    val res = XOR(es)(inf)
    add(inf.name, res)
    res

  final def xor(e1: Expr, e2: Expr, more: Expr*)(inf: Inf): XOR = xor(unique2(e1, e2, more *), inf)

  final def xor(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, falsy: String): XOR =
    xor(unique2(e1, e2, more *), Inf(name, truthy, falsy))

  final def xor(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, style: Style = Muted): XOR =
    xor(unique2(e1, e2, more *), Inf(name, truthy, style))

  final def xor(e1: Expr, e2: Expr, more: Expr*)(name: String): XOR =
    xor(unique2(e1, e2, more *), Inf(name, "", Muted))

  final def xor(e1: Expr, e2: Expr, more: Expr*): XOR = xor(e1, e2, more *)(Ano)

  /**
   * NAND
   *
   * @param es  inputs
   * @param inf name and predicate
   * @return
   */
  final def nand(es: Set[Expr], inf: Inf): NAND =
    val res = NAND(es)(inf)
    add(inf.name, res)
    res

  final def nand(e1: Expr, e2: Expr, more: Expr*)(inf: Inf): NAND = nand(unique2(e1, e2, more *), inf)

  final def nand(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, falsy: String): NAND =
    nand(unique2(e1, e2, more *), Inf(name, truthy, falsy))

  final def nand(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, style: Style = Muted): NAND =
    nand(unique2(e1, e2, more *), Inf(name, truthy, style))

  final def nand(e1: Expr, e2: Expr, more: Expr*)(name: String): NAND =
    nand(unique2(e1, e2, more *), Inf(name, "", Muted))

  final def nand(e1: Expr, e2: Expr, more: Expr*): NAND = nand(e1, e2, more *)(Ano)

  /**
   * NOR
   *
   * @param es
   * @param inf
   * @return
   */
  final def nor(es: Set[Expr], inf: Inf): NOR =
    val res = NOR(es)(inf)
    add(inf.name, res)
    res

  final def nor(e1: Expr, e2: Expr, more: Expr*)(inf: Inf): NOR = nor(unique2(e1, e2, more *), inf)

  final def nor(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, falsy: String): NOR =
    nor(unique2(e1, e2, more *), Inf(name, truthy, falsy))

  final def nor(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, style: Style = Muted): NOR =
    nor(unique2(e1, e2, more *), Inf(name, truthy, style))

  final def nor(e1: Expr, e2: Expr, more: Expr*)(name: String): NOR =
    nor(unique2(e1, e2, more *), Inf(name, "", Muted))

  final def nor(e1: Expr, e2: Expr, more: Expr*): NOR = nor(e1, e2, more *)(Ano)

  /**
   * NXOR
   *
   * @param es  inputs
   * @param inf name and predicate
   * @return
   */
  final def nxor(es: Set[Expr], inf: Inf): NXOR =
    val res = NXOR(es)(inf)
    add(inf.name, res)
    res

  final def nxor(e1: Expr, e2: Expr, more: Expr*)(inf: Inf): NXOR = nxor(unique2(e1, e2, more *), inf)

  final def nxor(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, falsy: String): NXOR =
    nxor(unique2(e1, e2, more *), Inf(name, truthy, falsy))

  final def nxor(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String, style: Style = Muted): NXOR =
    nxor(unique2(e1, e2, more *), Inf(name, truthy, style))

  final def nxor(e1: Expr, e2: Expr, more: Expr*)(name: String): NXOR =
    nxor(unique2(e1, e2, more *), Inf(name, "", Muted))

  final def nxor(e1: Expr, e2: Expr, more: Expr*): NXOR = nxor(e1, e2, more *)(Ano)

  /**
   * define EXCL for given expressions
   *
   * @param e1   first input
   * @param e2   second input
   * @param more additional inputs
   * @param name of node
   * @return
   */
  final def excl(e1: Expr, e2: Expr, more: Expr*)(name: String): EXCL = add(EXCL.apply(unique2Seq(e1, e2, more *) *), name)

  final def excl(e1: Expr, e2: Expr, more: Expr*): EXCL = excl(e1, e2, more *)("")

  /**
   * define INCL for given expressions
   *
   * @param e1   first input
   * @param e2   second input
   * @param more additional inputs
   * @param name of node
   * @return
   */
  final def incl(e1: Expr, e2: Expr, more: Expr*)(name: String): INCL = add(INCL.apply(unique2Seq(e1, e2, more *) *), name)

  final def incl(e1: Expr, e2: Expr, more: Expr*): INCL = incl(e1, e2, more *)("")

  /**
   * define ONE for given expressions
   *
   * @param e1   first input
   * @param e2   second input
   * @param more additional inputs
   * @param name of node
   * @return
   */
  final def one(e1: Expr, e2: Expr, more: Expr*)(name: String): ONE = add(ONE.apply(unique2Seq(e1, e2, more *) *), name)

  final def one(e1: Expr, e2: Expr, more: Expr*): ONE = one(e1, e2, more *)("")

  /**
   * define ANCHOR for a given expression
   *
   * @param e1   input
   * @param name of node
   * @return
   */
  final def anchor(e1: Expr)(name: String): ANCHOR = add(ANCHOR.apply(e1), name)

  final def anchor(e1: Expr): ANCHOR = anchor(e1)("")

  /**
   * define MASK for given expressions
   *
   * @param src  first input
   * @param e    first destination
   * @param more additional destination
   * @param name of node
   * @return
   */
  final def mask(src: Expr, e: Expr, more: Expr*)(name: String): MASK = add(MASK.apply(src, Seq(e) ++ more *), name)

  final def mask(src: Expr, e: Expr, more: Expr*): MASK = mask(src, e, more *)("")

  /**
   * define REQ for given expressions
   *
   * @param src  first input
   * @param e    first destination
   * @param more additional destination
   * @param name of node
   * @return
   */
  final def req(src: Expr, e: Expr, more: Expr*)(name: String): REQ = add(REQ.apply(src, Seq(e) ++ more *), name)

  final def req(src: Expr, e: Expr, more: Expr*): REQ = req(src, e, more *)("")

  /**
   * define NOTE for given nodes
   *
   * @param text of note
   * @param more linked nodes
   * @return
   */

  final def note(txt: String, more: (Expr | Constraint)*)(name: String): NOTE = add(NOTE.apply(txt, more *), name)

  final def note(txt: String, more: (Expr | Constraint)*): NOTE = note(txt, more *)("")

  /**
   * add a constraint
   *
   * @param name
   * @param contraint
   * @return
   */
  final def add(name: String, contraint: Constraint): Constraint =
    add(contraint, name)
    contraint

  final def add(name: String, note: NOTE): NOTE =
    add(note, name)
    note

  final def add(name: String, expr: Expr): Expr =
    add(expr, name)
    expr
end CEG

abstract class Child(implicit parent: CEG) extends CEG :
  /**
   * @inheritdoc
   */
  def root: Root = parent.root

  /**
   * @inheritdoc
   */
  def title: String = root.title

  /**
   * @inheritdoc
   */
  def add[T <: CeNode](x: T, name: String = ""): T = root.add(x, name)

  /**
   * @inheritdoc
   */
  def addX(node: CeNode, name: String): CeNode = root.addX(node, name)

  /**
   * @inheritdoc
   */
  def dict: Map[CeNode, String] = root.dict

  /**
   * @inheritdoc
   */
  def sequence: List[CeNode] = root.sequence

  /**
   * @inheritdoc
   */
  def pos(node: CeNode): Option[Pos] = root.pos(node)

  /**
   * @inheritdoc
   */
  def setXY(node: CeNode, x: Int, y: Int): Unit = root.setXY(node, x, y)

  /**
   * @inheritdoc
   */
  def setObs(expr: Expr, obs: Obs): Unit = root.setObs(expr, obs)

  /**
   * @inheritdoc
   */
  def setUi(expr: Expr, ui: String): Unit = root.setUi(expr, ui)
end Child

/**
 * Root for a Cause Effect Graph
 *
 * @param title
 */
class Root(val title: String = "") extends CEG :
  /**
   * @inheritdoc
   */
  given root: Root = this

  final def add[T <: CeNode](x: T, name: String = ""): T =
    x match {
      case x: CeNode => addX(x, name)
    }
    x

  private var nodesImpl = Map[CeNode, String]()
  private var namesImpl = Set[String]()
  private var revSequence: List[CeNode] = Nil
  private var xy: Map[CeNode, Pos] = Map()
  private var obs: Map[Expr, Obs] = Map()
  private var ui: Map[Expr, String] = Map()

  def reset(): Snap = reset(Snap(Map(), Set(), Nil, Map(), Map(), Map())(title))

  def reset(snap: Snap): Snap =
    val result = snap
    nodesImpl = snap.dict
    namesImpl = snap.names
    revSequence = snap.revSequence
    xy = snap.xy
    obs = snap.obs
    ui = snap.ui
    result

  /**
   * @inheritdoc
   */
  def pos(node: CeNode): Option[Pos] =
    Some(Pos(0, 0))

  /**
   * @inheritdoc
   */
  def sequence: List[CeNode] = revSequence.reverse

  /**
   * @inheritdoc
   */
  def setXY(node: CeNode, x: Int, y: Int): Unit = if nodesImpl.contains(node) then xy = xy + (node -> Pos(x, y))

  /**
   * @inheritdoc
   */
  def setObs(expr: Expr, obs: Obs): Unit = expr match {
    case _: CSE =>
    case _ => if nodesImpl.contains(expr) then this.obs = this.obs + (expr -> obs)
  }

  def setObs(expr: Expr, o: String): Unit = if nodesImpl.contains(expr) then o match {
    case "O" => setObs(expr, Observable)
    case "F" => setObs(expr, Forced)
    case "N" => setObs(expr, Normal)
    case _ => throw IllegalArgumentException(s"not and Obs state >>$o<< $expr")
  }

  /**
   * @inheritdoc
   */
  def setUi(expr: Expr, ui: String): Unit = if nodesImpl.contains(expr) then this.ui = this.ui + (expr -> ui)

  def last: Option[CeNode] = revSequence.headOption

  def snap: Snap = Snap(nodesImpl, namesImpl, revSequence, xy, obs, ui)(title)

  def reset(graph: Root): Snap = reset(graph.snap)

  def stash(comment: String = ""): Unit = ???

  def drop(): Unit = ???

  def pop(): Unit = ???

  /**
   * @inheritdoc
   */
  def addX(node: CeNode, name: String): CeNode =
    val trimmed = name.trim
    if contains(node) then {
      if (trimmed.nonEmpty && dict(node) != trimmed) {
        "attempt to add an existing node with a different name".WARN()
      }
    } else {
      val unique = if (trimmed.isEmpty) {
        if node.isInstanceOf[CSE] then numberedName(trimmed)
        else numberedName(node.getClass.getName.split("\\.").reverse.head)
      } else {
        uniqueName(trimmed)
      }
      revSequence = node :: revSequence
      nodesImpl = nodesImpl + (node -> unique)
      s"$unique ::- $node".DEBUG()
      // todo implement addNodes for constraints in NOTE
      node match {
        case x: NOTE =>
          addExprs(x.targets.collect { case t: Expr => t })
          addConstraints(x.targets.collect { case t: Constraint => t })

        case _ => addExprs(node.exprs)
      }
    }
    node

  def addConstraints(cs: Set[Constraint]): Unit = cs.foreach { n => if !nodesImpl.contains(n) then addX(n, "") }

  def addExprs(es: Set[Expr]): Unit = es.foreach { e => if !nodesImpl.contains(e) then addX(e, "") }

  final def dict: Map[CeNode, String] = nodesImpl

  private def uniqueName(proposal: String): String =
    if namesImpl.contains(proposal) then
      numberedName(proposal)
    else
      namesImpl = namesImpl + proposal
      proposal

  private def numberedName(proposal: String): String =
    @tailrec
    def recurse(i: Int): String =
      val name = s"${proposal}_$i"
      if (!namesImpl.contains(name))
        namesImpl = namesImpl + name
        name
      else recurse(i + 1)

    recurse(1)
end Root


object Root:

  def restore(snapshot: Snap, name: String): Root =
    val trimmed = name.trim
    val instance = if trimmed.isEmpty then new Root(snapshot.title) else new Root(trimmed)
    instance.namesImpl = snapshot.names
    instance.nodesImpl = snapshot.dict
    instance.revSequence = snapshot.revSequence
    instance

  def restore(snapshot: Snap): Root = restore(snapshot, "")

  def scalaName(str: String) = s"`$str`"
end Root // object
