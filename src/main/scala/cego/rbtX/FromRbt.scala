package cego.rbtX

import cego.*
import cego.graphX.*

import java.io.*
import scala.language.postfixOps
import scala.xml.{Node, NodeSeq, XML}
import cego.scalaX.*
def getMap( node:Node, bag:String="")(filter:String *):Map[String,String] =
  val items = bag match {
    case "" => node.child
    case _ =>   val tmp = node \ bag
      if tmp.isEmpty then Nil
      else
        assert( tmp.size==1 )
        tmp.head.child

  }
  val exclude =  filter.toSet + "#PCDATA"
  items.map( x => x.label -> x.text.trim).filterNot(x => exclude.contains(x._1 )).toMap

/**
 * import RBT files
 */
final class FromRbt(val doc: scala.xml.Node, fn: String = "") {
  println( "==== " + fn)
  given imp: FromRbt = this
  require(doc.label == "Document")

  val meta: Map[String, String] =
    val Ignore: Set[String] = Set("#PCDATA",
    "Title", "Nodes", "Constraints", "Relations", "Notes", "Variations",
    "CurrentRunSummary", "ProgressIndicator", "GraphMessages", "NewTestRun", "PreviousRunSummary")
    val subs = doc.child.filterNot(x => Ignore.contains(x.label))
    val res = subs.map(x => x.label -> x.text.trim).toMap
    res

  val title: D = D.fromRbt(doc)
  val Es: Seq[E] = E.fromRbt(doc)
  val Cs: Seq[C] = C.fromRbt(doc)
  val Rs: Seq[R] = R.fromRbt(doc)
  val Ns: Seq[N] = N.fromRbt(doc)
  val Vs: Seq[V] = V.fromRbt(doc)
  val Xs: Seq[X] = X.fromRbt(doc)
  val Ts: Seq[T] = T.fromRbt(doc)
  val Ms: Seq[M] = M.fromRbt(doc)
  val TsMeta: String = if Xs.isEmpty then ""
    else getMap( (doc \ "NewTestRun" \"Variations").head)("Variation").map( x => s"||${x._1}|${x._2}").mkString( "|===\n||Key|Value\n\n","\n","\n||===\n")

  val (ceg, exprs, constrains) =
    var eAcu: Map[String, Expr] = Map()
    var constraints = Map[String, Constraint]()

    def x(handle: String): Expr | Constraint = if eAcu.contains(handle) then eAcu(handle) else constraints(handle)

    val root: Root = Root(title.title)
    lazy val dummy = root.cse("DUMMY", "DUMMY is true", "DUMMY is false")
    def headOrDummy( es:Seq[Expr]) = if( es.nonEmpty) then es.head else dummy
    def someOrDummy( es:Option[Expr]) = if( es.nonEmpty) then es.head else dummy
    if meta("RelationLogicIsCircular") == "False" then {
      def isEffect(x: String) = x == "Primary Effect" || x == "Explicit Intermediate" || x == "Intermediate Effect"

      Es.filterNot(x => isEffect(x.typ)).foreach(x =>
        val name = if x.name.nonEmpty then x.name else mkUUID
        val cause = root.cse(name, x.truthy, x.falsy)
        root.setXY(cause, x.x.toInt, x.y.toInt)
        root.setObs(cause, x.obs)
        if x.ui != "None" then root.setUi(cause, x.ui)
        eAcu = eAcu + (x.handle -> cause)
      )
      val lookup = Es.filter(x => isEffect(x.typ)).map(x => x.handle -> x).toMap
      Rs.foreach { r =>
        val pe = lookup(r.handle)
        val falsy = pe.falsy
        val truthy = pe.truthy
        val name = pe.name
        val targets = r.states.map(state =>
          state.state match {
            case "T" => eAcu(state.id)
            case "F" => root.add(NOT(eAcu(state.id)))
          }
        )
        val t = r.typ match {
          case "&" => root.add( AND(targets*)(truthy,falsy), name)
          case "|" => root.add( OR(targets*)(truthy,falsy), name)
          case "%" => root.add( XOR(targets*)(truthy,falsy), name)
          case "^" => root.add( NXOR(targets*)(truthy,falsy), name)
          case "#" => root.add( NOR(targets*)(truthy,falsy), name)
          case "@" => root.add( NAND(targets*)(truthy,falsy), name)
          case "$" => root.eq(headOrDummy(targets))(name, truthy,  falsy)
        }
        root.setXY(t, pe.x.toInt, pe.y.toInt)
        if pe.ui != "None" then root.setUi(t, pe.ui)
        eAcu = eAcu + (r.handle -> t)
        t match {
          case NOT(x) => eAcu = eAcu + (r.handle -> x)
          case _ => eAcu = eAcu + (r.handle -> t)
        }
      }
      Cs.foreach { c =>
        val targets = c.states.map(state =>
          state.state match
            case "T" => eAcu(state.id)
            case "F" => root.add(NOT(eAcu(state.id)))
        )
        val ref: Option[Expr] =
          val id = c.sbj
          if id == "0" then None
          else c.state match
            case "T" => Some(eAcu(id))
            case "F" => Some(root.add(NOT(eAcu(id))))
            case "X" => None // TODO check if this mapping is ok

        val t = c.typ match
          case "E" => root.add( EXCL(targets *))
          case "I" => root.add(INCL(targets * ))
          case "O" => root.add(ONE(targets *))
          case "M" => root.add(MASK(someOrDummy(ref), targets *))
          case "R" => root.add(REQ(someOrDummy(ref), targets *))
          case "A" => root.anchor(someOrDummy(ref))

        root.setXY(t, c.x.toInt, c.y.toInt)
        constraints = constraints + (c.handle -> t)
      }
      Ns.foreach { n =>
        val targets = n.links.map(handle => x(handle))
        val t = root.note(n.text, targets *)
        root.setXY(t, n.x.toInt, n.y.toInt)
      }
    }
    (root, eAcu, constraints)

  def snap: Snap = ceg.snap
  def sequence: Seq[CeNode] = snap.sequence

  def exprName(handle: String): String = snap.dict(exprs(handle))

  def exprDesc(handle: String, state: String): String =
    val name = exprName(handle)
    val expr = exprs(handle)
    val str = state match {
      case "T" =>
        if expr.truthy.nonEmpty then expr.truthy else name
      case "F" =>
        if expr.falsy.nonEmpty then expr.falsy
        else if expr.truthy.nonEmpty then "*NOT* " + expr.truthy
        else "*NOT* " + name
    }
    s"${if state == "T" then YES else NO} $name =>  $str"

  def toAdoc: String = {
    val gv = ToGraphviz(snap)
    val pic = gv.mkAdocPic
    val leg = gv.mkLegend
    val pic2 = ToGraphviz(snap, true).mkAdocPic
    val nodes = leg.keySet.toList.sorted.map(x => s"* ${leg(x)} $x").mkString("\n")

    val res = snap.sequence.filterNot(_.isInstanceOf[NOT])
    val causes = snap.sequence.filter(_.isInstanceOf[CSE])

    "VARIATIONS grouped".hint.INFO()
    val grouped: Map[String, Seq[V]] = Vs.groupBy(_.effect)
    val vStr =
      val res = for (k -> vs <- grouped) yield {
        s"\n\n=== ${exprName(k)}\n....\n${vs.head.mkNames}\n....\n${vs.map(_.toAdoc).mkString}"
      }
      res

    val ById =
      s"""
         |== By id
         |
         |$pic
         |""".stripMargin

    s""":doctyp: book
       |:toc: left
       |:toclevels: 4
       |
       |= ${title.title}
       |
       |== Overview
       |
       | $fn
       |
       |$pic2
       |
       |== Nodes
       |
       |$nodes
       |
       |== Variations
       |
       |${vStr.mkString}
       |
       |== Test Run Variations
       |
       |$TsMeta
       |
       |${Xs.map( x => s"\n * $x" ).mkString("\n")}
       |
       |${Xs.map(_.toAdoc).mkString("\n")}
       |
       |== Test Run Tests
       |
       |${Ts.map(t => s"\n * $t" ).mkString("\n")}
       |
       |${Ts.map(_.toAdoc).mkString("\n")}
       |
       |== Message
       |
       |${Ms.map( m => s"\n * $m").mkString("\n")}
       |
       |== Statistics
       |
       |* Number of causes:     ${causes.size}
       |
       |* Theoretical number of test cases: 2 ^ ${causes.size} = ${Math.pow(2.0, causes.size)}
       |
       |* Compression rate:  ${(Math.pow(2.0, causes.size) / Ts.size * 100).toInt / 100.0}
       |
       |* 8Number of variations: ${Vs.size}
       |
       |* Number of tests:       ${Ts.size}
       |
       |=== CurrentRunSummary
       |
       |${getMap(doc, "CurrentRunSummary")().map( x => s"||${x._1}|${x._2}").mkString( "|===\n||Key|Value\n\n","\n","\n||===\n")}
       |
       |=== PreviousRunSummary
       |
       |${getMap(doc, "PreviousRunSummary")().map( x => s"||${x._1}|${x._2}").mkString( "|===\n||Key|Value\n\n","\n","\n||===\n")}
       |
       |=== ProgressIndicator
       |
       |${getMap(doc, "ProgressIndicator")().map( x => s"||${x._1}|${x._2}").mkString( "|===\n||Key|Value\n\n","\n","\n||===\n")}
       |
       |== Meta
       |
       |${meta.map( x => s"||${x._1}|${x._2}").mkString( "|===\n||Key|Value\n\n","\n","\n||===\n")}
       |
       |
       |""".stripMargin
  }
}

//     "CurrentRunSummary", "ProgressIndicator", "GraphMessages", "NewTestRun", "PreviousRunSummary")
object FromRbt:

  def load(fn: String): FromRbt =
    val xml = XML.loadFile(fn).head
    val doc = xml.head
    new FromRbt(doc, fn)

  def analyze(fn: String): Unit =
    val f = File(fn)
    val files = if f.isFile then Seq(fn) else f.listFiles().toList.map(_.getPath).filter(_.endsWith(".rbt"))
    files.foreach(f => assert(f.endsWith(".rbt"), s"must end with .rbt $f"))
    var problems:List[Throwable] = Nil
    for (f <- files.map(_.dropRight(4))) {
      try {
        val imp = FromRbt.load(s"$f.rbt")
        val snap = imp.snap
        ToRbt(snap).toXML
        Root.restore(snap)
        val code = ToScala(snap).toString
        code.writeTo(s"$f.sc")
        imp.toAdoc.writeTo(s"$f.adoc")
      } catch {
        case x: Exception =>
          problems = x :: problems
          x.printStackTrace(System.err)
          System.err.flush()
          System.out.flush()
          System.out.println( x.getMessage )
      }
    }
    problems.reverse.foreach(println)
    println ( s"Processed ${files.size} Files. Error(s): ${problems.size}")

  def main(args: Array[String]): Unit =
    if args.length != 1 then println("Usage Import filename") else analyze(args(0))