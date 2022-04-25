package cego.rbtX

import cego.*

import scala.xml.{Node, NodeSeq, XML}

def getStates(node: Node) =
  val zip = (node \ "States" \ "State").zip(node \ "States" \ "Node")
  zip.map( z => S(z._2.text, z._1.attribute("Handle").head.text, z._1.text))

def getLinkedHandles(node: Node) =
  val zip = (node \ "LinkedNodes" \ "Node").zip(node \ "LinkedNodes" \ "NodeHandle")
  zip.map( z=> z._2.text)

def noB(s: String): String = if s.toUpperCase() == "/B" then "" else s

extension (node: scala.xml.Node)

/**
 * get a single child node
 */
  def one(tag: String) =
    val res = node \ tag
    assert(res.size == 1, s"tag $tag is missing")
    res.head
  /**
   * get nodes from a container
   *
   * @param name    of tag
   * @param bagName append s to tag if empty
   * @return
   */
  def bag(name: String, bagName: String = ""): (Node, NodeSeq) =
    val names = if (bagName.isEmpty) name + "s" else bagName
    val bag = node \ names
    //s"${bag.size}\t=> $names".INFO()
    val res = bag \ name
    //s"${res.size}\t=> $name".INFO()
    if bag.isEmpty then s"Empty bag $names $name".WARN()
    (bag.head, res)

  def text(name: String) = node.one(name).text.trim
  def texts(names: String*) = names.map(x => node.one(x).text.trim)


case class S(node: String, id: String, state: String):
  assert(id.toLong > 0)
  assert(Set("T", "F", "M", "I").contains(state.toUpperCase()), s"expected T,F, M found $state")

val NO: String = "✘"
val YES: String = "✔"


trait Helper:
  def asString: String = toString

/**
 * document / rbt file information
 *
 * @param title   of document
 * @param author  of document
 * @param date    of document
 * @param rev     of document
 * @param comment for document
 */
case class D(title: String, author: String, date: String, rev: String, comment: String)(implicit val imp: FromRbt)

object D:
  val c: Seq[String] = Seq("TitleString", "Author", "CreationDate", "Revision", "Comment")

  def fromRbt(doc: Node)(implicit imp: FromRbt): D =
    val n = doc.one("Title")
    val s0 :: s1 :: s2 :: s3 :: s4 :: _ = n.texts(c *)
    D(s0, s1, s2, s3, s4)


/**
 * Cause/Effect
 *
 * @param handle <Handle>
 * @param name   <Name>
 * @param truthy <TrueDescription>
 * @param falsy  <FalseDescription>
 * @param ui     <UiType>
 * @param x      <X>
 * @param y      <Y>
 * @param obs    <cObservability>
 * @param typ    <sNodeType>
 */
case class E(handle: String, name: String, truthy: String, falsy: String, ui: String, x: String, y: String, obs: String, typ: String)(implicit val imp: FromRbt):
  assert(handle.toLong > 0)
  assert(Set("Primary Effect", "Primary Cause", "Orphan", "Explicit Intermediate", "Intermediate Effect").contains(typ), s"typ $typ unknown")

object E:
  val c: Seq[String] = Seq("Handle", "Name", "TrueDescription", "FalseDescription", "UiType", "X", "Y", "cObservability", "sNodeType")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[E] =
    val ns = doc.bag("Node")
    def avoidEmpty( s:String):String = if s.nonEmpty then s else java.util.UUID.randomUUID().toString

    ns._2.map{ n =>
      val s0 :: s1 :: s2 :: s3 :: s4 :: s5 :: s6 :: s7 :: s8 :: _ = n.texts(c *)
      E(s0, avoidEmpty(s1), s2, noB(s3), s4, s5, s6, s7, s8)
    }

/**
 * Constraint
 *
 * @param handle <Handle>
 * @param typ    <cConstraintTyp>
 * @param sbj    <Subject>
 * @param state  <cSubjectState>
 * @param x      <X>
 * @param y      <Y>
 * @param states <States> ( <Node>half</Node><State Handle="192323">F</State> ) *
 */
case class C(handle: String, typ: String, sbj: String, state: String, x: String, y: String, states: S*)(implicit val imp: FromRbt):
  assert(handle.toLong > 0)
  assert(Set("E", "I", "O", "M", "R", "A").contains(typ), s"typ $typ unknown")

object C:
  val c:Seq[String] = Seq("Handle", "cConstraintType", "Subject", "cSubjectState", "X", "Y")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[C] =
    val ns = doc.bag("Constraint")
    ns._2.map{ n =>
      val s0 :: s1 :: s2 :: s3 :: s4 :: s5 :: _ = n.texts(c *)
      C(s0, s1, s2, s3, s4, s5, getStates(n) *)
    }

/**
 *
 * @param handle <Handle> OUTPUT DATA effect node name, human readable FOR DEBUGGING
 * @param name
 * @param effect <Effect>
 * @param typ    <cOperator> a single character representing the relation operator. The options are:
 * @param active <cActiveOrPassive> for future use
 * @param states <States> ( <Node>half</Node><State Handle="192323">F</State> ) *
 */
case class R(handle: String, name: String, effect: String, typ: String, active: String, states: S*)(implicit val imp: FromRbt):
  assert(handle.toLong > 0)
  assert(Set("&", "|", "%", "@", "#", "^", "$").contains(typ))

object R:
  val c: Seq[String] = Seq("EffectNode", "Handle", "Effect", "cOperator", "cActiveOrPassive")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[R] =
    val ns = doc.bag("Relation")
    ns._2.map{ n =>
      val s0 :: s1 :: s2 :: s3 :: s4 :: _ = n.texts(c *)
      R(s1, s0, s2, s3, s4, getStates(n) *)
    }

/**
 * Note
 *
 * @param handle <Handle>
 * @param text   <Text>
 * @param x      <X>
 * @param y      <Y>
 * @param links  <LinkedNodes Count="2"> ( <Node>SE</Node><NodeHandle>239929</NodeHandle> ) *
 */
case class N(handle: String, text: String, x: String, y: String, links: String*)(implicit val imp: FromRbt):
  require(handle.toLong > 0)
  links.map(_.toInt)

object N:
  val c: Seq[String] = Seq("Handle", "Text", "X", "Y")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[N] =
    val ns = doc.bag("Note")
    ns._2.map{ n =>
      val s0 :: s1 :: s2 :: s3 :: _ = n.texts(c *)
      val les: Seq[String] = if (n \ "LinkedNodes").nonEmpty then getLinkedHandles(n) else Seq()
      val lcs: Seq[String] = if (n \ "LinkedConstraints").nonEmpty then n.bag("ConstraintHandle", "LinkedConstraints")._2.map(_.text)
      else Seq()

      N(s0, s1, s2, s3, les ++ lcs *)
    }

/**
 * Variation
 *
 * @param handle <Handle> Variation handle
 * @param node   <EffectNode> effect node name, human readable FOR DEBUGGING
 * @param effect <Effect> effect node handle
 * @param state  <cEffectState> effect node state, T or F
 * @param oper   <cOperator> a single character representing the relation operator
 * @param states <States> ( <Node>half</Node><State Handle="192323">F</State> ) *
 */
case class V(handle: String, node: String, effect: String, state: String, oper: String, msgs: Seq[M], states: Seq[S])(implicit val imp: FromRbt):
  assert(handle.toLong > 0)

  def mkStateDesc: String =
    def mkOper(s: String) = if s == "T" then "" else "~ "

    def mapStates = states.map(s => s"${s.state}").mkString("(", ",", ")")

    s"$node $mapStates"

  def mkNames: String = states.map(s => s"${s.node}").mkString("( ", ", ", " )")

  def mkHeading: String =
    states.map{ s=>
      val tmp = if s.state == "T" then YES else NO
      val name = imp.exprName(s.id).take(3).mkString
      s"$tmp$name"
    }.mkString("|")

  def toAdoc: String =
    def mkIt: String = states.map { i=>("* " + imp.exprDesc(i.id, i.state)).mkString}.mkString("\n")
    s"\n\n==== $state $mkHeading\n\n$mkIt\n"

object V:
  val c: Seq[String] = Seq("Handle", "EffectNode", "Effect", "cEffectState", "cOperator")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[V] =
    if( (doc \ "Variations").isEmpty ) Nil
    else {
      val ns = doc.bag("Variation")
      for (n <- ns._2) yield {
        val s0 :: s1 :: s2 :: s3 :: s4 :: _ = n.texts(c *)
        val vms = if (n \ "VariationMessages").nonEmpty then
          val c = List("MessageString", "sCode", "LineNumber", "Subgraph", "Type")
          val ms = doc.bag("Message", "VariationMessages")
          ms._2.map { m=>
            val s0 :: s1 :: s2 :: s3 :: s4 :: _ = m.texts(c *)
            M(s0, s1, s2, s3, s4)
          }
        else Nil

        V(s0, s1, s2, s3, s4, vms, getStates(n))
      }
    }

/**
 * Graph Message
 *
 * @param msg  <MessageString> the text of the message
 * @param code <sCode> message identifier
 * @param line <LineNumber> for future use
 * @param sub  <Subgraph> for future use
 * @param typ  <Type> FOR DEBUGGING
 */
case class M(msg: String, code: String, line: String, sub: String, typ: String)(implicit val imp: FromRbt)

object M:
  val c: Seq[String] = Seq("MessageString", "sCode", "LineNumber", "Subgraph", "Type")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[M] =
    if (doc \ "GraphMessages").nonEmpty then
      val ns = doc.bag("Message", "GraphMessages")
      ns._2.map{ n =>
        val s0 :: s1 :: s2 :: s3 :: s4 :: _ = n.texts(c *)
        M(s0, s1, s2, s3, s4)
      }
    else Nil

/**
 * variation within new testRun
 *
 * @param handle      <Handle>
 * @param node        <EffectNode> effect node name, human readable FOR DEBUGGING
 * @param effect      <Effect> effect node handle
 * @param coverage    <CoverageCount> number of tests that cover this variation
 * @param reason      <Reason> why this variation cannot be covered
 * @param feasibility <Feasibility> feasible, infeasible, untestable or not tested
 * @param inUse       <IsInUse> for future use
 * @param msgs        <VariationMessages> * list of messages attached to this variation
 */
case class X(handle: String, node: String, effect: String, coverage: String, reason: String, feasibility: String, inUse: String, msgs: Seq[M])(implicit val imp: FromRbt):
  def toAdoc: String =
    s"=== ${imp.exprName(effect)}\n* Feas: $feasibility\n* Reason: $reason\n* Coverage: $coverage\n* InUse: $inUse\n" +
      msgs.map { st =>s"* ${st.typ} ${st.code} ${st.msg}\n"}.mkString

object X:
  val vc: Seq[String] = Seq("Handle", "EffectNode", "Effect", "CoverageCount", "Reason", "Feasibility", "IsInUse")
  val fields: Seq[String] = Seq("MessageString", "sCode", "LineNumber", "Subgraph", "Type") // TODO can be reused from M

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[X] =
    val run = doc \ "NewTestRun"
    if run.isEmpty then Nil
    else
      val vs = run.head.bag("Variation")
      val vsr = for (n <- vs._2) yield {
        val s0 :: s1 :: s2 :: s3 :: s4 :: s5 :: s6 :: _ = n.texts(vc *)
        val msgs = if (n \ "VariationMessages").nonEmpty then
          val ms = n.bag("Message", "VariationMessages")
          ms._2.map{ m =>
            val s0 :: s1 :: s2 :: s3 :: s4 :: _ = m.texts(fields *)
            M(s0, s1, s2, s3, s4)
          }
        else Nil

        X(s0, s1, s2, s3, s4, s5, s6, msgs)
      }
      vsr

/**
 * Testcase in new testRun.
 *
 * @param handle                        <Handle> a randomly generated prime number used internally as an identifier
 * @param name                          <Name> this is the base test name plus the test index
 * @param old                           <Old> is this an old test?
 * @param Manual                        <Manual> was this test created by hand in the Test Editor?
 * @param UniquelyCoveredVariationCount <UniquelyCoveredVariationCount> number of variations that are only covered by this test.
 * @param TestIsValid                   <TestIsValid> if it is an old test, it could be invalid
 * @param Passed                        <Passed> has this test been marked as "Passed" in the Coverage Analyzer?
 * @param states                        <States> *
 *                                      <Node> node name. human readable FOR DEBUGGING
 *                                      <State> node handle and node state, T or F.  Example:
 *                                      <State Handle="143137">T</State>
 * @param covs                          <Covered Variations> *
 *                                      <Index> Variation handle
 */
case class T(handle: String, name: String, old: String, Manual: String, UniquelyCoveredVariationCount: String, TestIsValid: String, Passed: String, states: Seq[S], covs: Seq[String])(implicit val imp: FromRbt):
  def toAdoc: String =
    s"=== $name\n" +
      states.map { st => s"* ${st.state} ${imp.exprName(st.id)}\n"}.mkString

object T:
  val tc: Seq[String] = Seq("Handle", "Name", "Old", "Manual", "UniquelyCoveredVariationCount", "TestIsValid", "Passed")

  def fromRbt(doc: Node)(implicit imp: FromRbt): Seq[T] =
    val run = doc \ "NewTestRun"
    if run.isEmpty then Nil
    else
      val ts = run.head.bag("Test")
      ts._2.map { n =>
        val s0 :: s1 :: s2 :: s3 :: s4 :: s5 :: s6 :: _ = n.texts(tc *)
        val states = getStates(n)
        val cv = n.bag("Index", "CoveredVariations")
        val covs = cv._2.map(_.text)
        T(s0, s1, s2, s3, s4, s5, s6, states, covs)
      }
