package cego

import scala.annotation.tailrec

/**
 *
 * @param truthy True case description (trimmed)
 * @param falsy False state description (trimmed)
 */
case class Pred private(truthy:String, falsy:String){
  /**
   * supressed
   * @return
   */
  def muted:Boolean = falsy == Pred.Ign
  def empty:Boolean = truthy.isEmpty && falsy.isEmpty
  def negated:Boolean = truthy.nonEmpty && falsy.isEmpty
  def full:Boolean = truthy.nonEmpty && falsy.nonEmpty && !muted

  //  def toScala:String = ""
}

enum Style:
  case Explicit
  case Negated
  case Muted

/**
 * Describe truthy and falsy state.
 * Becomes Negated in case of falsy.isEmpty
 */
val Explicit = Style.Explicit

/**
 * Describe falsy state as NOT truthy in case of falsy.isEmpty
 */
val Negated = Style.Negated

/**
 * Mute falsy state in case of falsy.isEmpty
 */
val Muted = Style.Muted


object Pred{
  val Ign = "/b"
  val Neg = ""
  def apply(truthy:String , falsy:String ):Pred =
    def nrm( s:String) = s.trim match { case "/B"=>"/b"; case x => x} // FIXME remove multiple whitespace
    (nrm(truthy), nrm(falsy)) match{
      case ("","") => new Pred("","")
      case ("",Ign) => new Pred("",Ign)
      case (Ign,Ign) => new Pred("",Ign)
      case (Ign,"") => new Pred("",Ign)
      case (t,f) if t == f => new Pred( t, s"$t isn't true")
      case (t,f) => new Pred( t,f)
    }

  /**
   *
   * @param truthy description for truthy
   * @param style if true NOT truthy else suppress
   * @return
   */
  def apply(truthy:String="", style:Style = Muted ):Pred =
    if style == Negated then apply(truthy, Neg)  else apply(truthy, Ign)
}

val Empty = Pred("","")


/**
 * inhabitants of a CEGraph derive from here
 */
sealed abstract class CeNode:
  /**
   * dependencies for this Node
   * @return
   */
  def exprs: Set[Expr]

  def pred: Pred

  def rank: Int

/**
 * root for all logical nodes is, not , and, or, ... more to come
 */
sealed abstract class Expr extends CeNode :
  /**
   * description of true state
   * @return
   */
  final def truthy: String = pred.truthy

  /**
   * description of false state
   * @return
   */
  final def falsy: String = pred.falsy

  /**
   * negation of true state by prefixing in with "NOT"
   * @return
   */
  final def not: String = s"NOT $truthy".trim


  // optimizing operators
  final def unary_~ : Expr = NOT(this).optimized

  final def &(that: Expr): AND = AND(this, that).optimized

  final def |(that: Expr): OR = OR(this, that).optimized

  // non optimizing
  final def unary_! : Expr = NOT(this)

  final def &&(that: Expr): AND = AND(this, that)

  final def ||(that: Expr): OR = OR(this, that)

  def logic(inverted: Boolean = false): (Expr, Boolean) = (this, !inverted) // only not is different

object Expr:
  val DUMMY = "This is a true statement"

/**
 * @param pred
 * truthy darf hier nicht leer sein, daher die Einschränkung auf Half und Full
 * Aussage dürfen nicht mit keywords beginnen oder solche enthalten
 * keine mehrfachen whitespace enthalten
 * getrimmed
 * truthy muss ungleich falsy sein, ausnahme beide empty
 */
case class CSE (pred: Pred) extends Expr :
//  pred  match {
//    case Empty => throw IllegalArgumentException("Cause without truthy")
//  }

  def exprs: Set[Expr] = Set()

  final def rank: Int = 2

object CSE:
  def apply(truthy: String, falsy:String): CSE = new CSE( Pred(truthy, falsy) )
  def apply(truthy:String="", style:Style = Muted ):CSE = new CSE( Pred(truthy, style))
/**
 * base class for all unary expressions
 */
abstract class Unary extends Expr :
  def expr: Expr

  def exprs: Set[Expr] = Set(expr)

/**
 * NOT is always anonymous
 *
 * @param expr to be inverted
 */
case class NOT private(expr: Expr) extends Unary :
  def pred: Pred = Empty

  final def rank: Int = expr.rank

  override def logic(inverted: Boolean = false): (Expr, Boolean) =
    val tmp = expr.logic(inverted)
    (tmp._1, !tmp._2)

  def optimized: Expr = expr match {
    case NOT(x) => x // reduce double negation
    case _ => this
  }

object NOT:
  def apply(e: Expr) = new NOT(e)

/**
 * Simple Node.
 * Predicate is part of the c'tor /case class to allow distinct effect on identical logic
 *
 * @param expr related expression for this node
 * @param pred related description for this node
 */
case class EQ (expr: Expr, pred: Pred) extends Unary :
  final def rank: Int = expr.rank + 1


object EQ:
  def apply(expr: Expr)(truthy: String = Expr.DUMMY, falsy: String = ""): EQ =
      new EQ(expr,Pred(truthy, falsy))

  def apply(expr: Expr): EQ = apply(expr)()

/**
 * simple inverted EQ node.
 * Not sure if this makes really sense
 * Just for symmetric reasons like AND / NAND
 *
 * @param expr related expression for this node.
 * @param pred description for this node
 */
case class NEQ private(expr: Expr, pred: Pred) extends Unary :
  final def rank: Int = expr.rank + 1

object NEQ:
  def apply(expr: Expr)(truthy: String = Expr.DUMMY, falsy: String = ""): NEQ =
    new NEQ(NOT(expr),Pred(truthy, falsy) )

/**
 * base class for n-ary Expression
 */
abstract class Multi extends Expr :
  final def rank: Int =
    if exprs.isEmpty then 2
    else exprs.map(_.rank).max + 1


/**
 * conjunction of expressions.
 * AND with no input is true by default
 * AND with one input is state of input
 *
 * @param exprs inputs
 * @param pred  descriptions
 */
case class AND(exprs: Set[Expr])(val pred: Pred) extends Multi :
  def optimized: AND =
    val ands = exprs.filter(x => x.isInstanceOf[AND] && x.pred == Empty)
    val rest = exprs -- ands
    new AND(ands.flatMap(_.exprs) ++ rest)(Empty)

object AND:
  def apply(es: Expr*)(pred: Pred): AND = new AND(unique(es *))(pred)

  def apply(es: Expr*): AND = apply(es *)(Empty)

  def apply(es: Expr*)(truthy: String, falsy: String = ""): AND = apply(es *)(Pred(truthy, falsy))

/**
 * disjunction of expressions.
 * OR with no input is false by default
 * OR with one input is state of input
 *
 * @param exprs inputs
 * @param pred  descriptions
 */
case class OR(exprs: Set[Expr])(val pred: Pred) extends Multi :
  def optimized: OR =
    val ors = exprs.filter(x => x.isInstanceOf[OR] && x.pred == Empty)
    val rest = exprs -- ors
    new OR(ors.flatMap(_.exprs) ++ rest)(pred)

object OR:
  def apply(es: Expr*)(pred: Pred): OR = new OR(unique(es *))(pred)

  def apply(es: Expr*): OR = apply(es *)(Empty)

  def apply(es: Expr*)(truthy: String, falsy: String = ""): OR = apply(es *)(Pred(truthy, falsy))

/**
 * exclusive disjunction of  expressions.
 * XOR with no input is false by default
 * XOR with one input is state of input
 *
 * @param exprs inputs
 * @param pred  descriptions
 */
case class XOR(exprs: Set[Expr])(val pred: Pred) extends Multi

object XOR:
  def apply(es: Expr*)(pred: Pred): XOR = new XOR(unique(es *))(pred)

  def apply(es: Expr*): XOR = apply(es *)(Empty)

  def apply(es: Expr*)(truthy: String, falsy: String = ""): XOR = apply(es *)(Pred(truthy, falsy))

/**
 * inverted conjunction of expressions.
 * NAND with no input is false by default
 * NAND with one input is NOT input
 *
 * @param exprs inputs
 * @param pred  descriptions
 */
case class NAND(exprs: Set[Expr])(val pred: Pred) extends Multi

object NAND:
  def apply(es: Expr*)(pred: Pred): NAND = new NAND(unique(es *))(pred)

  def apply(es: Expr*): NAND = apply(es *)(Empty)

  def apply(es: Expr*)(truthy: String, falsy: String = ""): NAND = apply(es *)(Pred(truthy, falsy))

/**
 * inverted disjunction of expressions.
 * NOR with no input is by default true
 * NOR with one input is NOT input
 *
 * @param exprs inputs
 * @param pred  descriptions
 */

case class NOR(exprs: Set[Expr])(val pred: Pred) extends Multi

object NOR:
  def apply(es: Expr*)(pred: Pred): NOR = new NOR(unique(es *))(pred)

  def apply(es: Expr*): NOR = apply(es *)(Empty)

  def apply(es: Expr*)(truthy: String, falsy: String = ""): NOR = apply(es *)(Pred(truthy, falsy))

/**
 * inverted exclusive disjunction of expressions.
 * NXOR with no input is by default true
 * NXOR with one input is NOT input
 *
 * @param exprs inputs
 * @param pred  descriptions
 */

case class NXOR(exprs: Set[Expr])(val pred: Pred) extends Multi

object NXOR:
  def apply(es: Expr*)(pred: Pred): NXOR = new NXOR(unique(es *))(pred)

  def apply(es: Expr*): NXOR = apply(es *)(Empty)

  def apply(es: Expr*)(truthy: String, falsy: String = ""): NXOR = apply(es *)(Pred(truthy, falsy))


/**
 * base class for all constraints
 */
sealed abstract class Constraint extends CeNode :
  def pred: Pred = Empty

  final def rank: Int = 1

  def dests: Set[Expr]

/**
 * exclusion of a set of nodes. At most one
 *
 * @param exprs
 */
case class EXCL(exprs: Set[Expr]) extends Constraint :
  def dests: Set[Expr] = exprs

object EXCL:
  def apply(es: Expr*): EXCL = new EXCL(unique(es *))

/**
 * exactly one of a set
 *
 * @param exprs
 */
case class ONE(exprs: Set[Expr]) extends Constraint :
  def dests: Set[Expr] = exprs

object ONE:
  def apply(es: Expr*): ONE = new ONE(unique(es *))

/**
 * incl
 *
 * @param exprs
 */
case class INCL(exprs: Set[Expr]) extends Constraint :
  def dests: Set[Expr] = exprs

object INCL:
  def apply(es: Expr*): INCL = new INCL(unique(es *))

/**
 * anchor
 *
 * @param expr
 */
case class ANCHOR(expr: Expr) extends Constraint :
  def exprs: Set[Expr] = Set(expr)

  def dests: Set[Expr] = exprs

/**
 * mask
 *
 * @param src input
 * @param dests outputs
 */
case class MASK private(src: Expr, dests: Set[Expr]) extends Constraint :
  def exprs: Set[Expr] = dests + src

object MASK:
  def apply(src: Expr, dests: Expr*): MASK =
    val clean = ensureNoDuplicates(dests)
    assert(!clean.contains(src))
    new MASK(src, clean)

/**
 * req
 *
 * @param src input
 * @param dests output
 */
case class REQ private(src: Expr, dests: Set[Expr]) extends Constraint :
  def exprs: Set[Expr] = dests + src

object REQ:
  def apply(src: Expr, dests: Expr*): REQ =
    val clean = ensureNoDuplicates(dests)
    assert(!clean.contains(src))
    new REQ(src, clean)


/**
 * textual annotations can be linked to other nodes
 *
 * @param text    descriptive text of this node (can be multiline but not empty; will be trimmed)
 * @param targets optional nodes related to this note
 */
case class NOTE private(text: String, targets: Set[Expr | Constraint]) extends CeNode :
  def pred: Pred = Empty

  def exprs: Set[Expr] = Set()

  def rank: Int = 0

object NOTE:
  def apply(text: String = "TODO enter description ...", args: (Expr | Constraint)*): NOTE =
    val trimmed = text.trim
    if trimmed.isEmpty then throw IllegalArgumentException("Note is empty")
    new NOTE(trimmed, ensureNoDuplicates(args))
