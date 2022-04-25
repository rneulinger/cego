package cego

import jdk.incubator.vector.VectorOperators.Unary



/**
 *
 * @param dict            map each node to a unique name
 * @param names           companion to dict to ensure uniqueness of values
 * @param revSequence order in which nodes were added
 * @param xy              position for each node
 * @param title           of graph
 */
case class Snap(dict: Map[CeNode, String], names: Set[String], revSequence: List[CeNode], xy: Map[CeNode, Pos], obs: Map[Expr, Obs], ui: Map[Expr, String])(val title: String) extends MixIn :
  require(dict.values.toSet == names) // sanity checks
  require(dict.keySet == revSequence.toSet)
  require(dict.keySet.size == revSequence.size)

  lazy val sequence: List[CeNode] = revSequence.reverse

  /**
   * get position of a node
   *
   * @param node
   * @return position of node if defined and if node is part of the nodeSet.
   */
  final def pos(node: CeNode): Option[Pos] = if xy.contains(node) then Some(xy(node)) else None

  /**
   * find out all expressions not used by other expressions
   */
  final def effects(): Set[Expr] =
    var used = Set[Expr]()
    expressions.foreach(x => used = used ++ x.exprs)
    constraints.foreach(c =>
      used = used ++ c.exprs;
      c match {
        case x: ANCHOR => used = used + x.expr
        case x: MASK => used = used + x.src
        case x: REQ => used = used + x.src
        case _ =>
      }
    )
    val result = expressions -- used -- causes.toSet[Expr]
    result

  /**
   *
   * @param n
   */
  final def filter(n: CeNode): Set[CeNode] =
    dict.keySet.filter(_.getClass == n.getClass)

  def short( ceNode: CeNode):String =
    def child( n:CeNode):String =
      n match{
        case x:NOT => "~"+child( x.expr)
        case x => dict(x)
      }
    if ! dict.keySet.contains(ceNode) then throw IllegalArgumentException( s"not a member of this graph:$ceNode")
    ceNode match{
      case x:CSE => dict(x)
      case x:AND => x.exprs.map( x => child(x)).mkString( "AND( ", ", ", " )")
      case x:OR => x.exprs.map( x => child(x)).mkString( "OR( ", ", ", " )")
      case x:XOR => x.exprs.map( x => child(x)).mkString( "XOR( ", ", ", " )")
      case x:NAND => x.exprs.map( x => child(x)).mkString( "NAND( ", ", ", " )")
      case x:NOR => x.exprs.map( x => child(x)).mkString( "NOR( ", ", ", " )")
      case x:NXOR => x.exprs.map( x => child(x)).mkString( "NXOR( ", ", ", " )")
      case x:EQ => x.exprs.map( x => child(x)).mkString( "EQ( ", ", ", " )")
      case x:NOT => "NOT"
      case _ => dict(ceNode)
    }