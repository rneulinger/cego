package cego.rbtX

import cego.*

import scala.xml.*

/**
 * export to RBT.
 * implements basic layout in case of missing xy information
 * @param snapshot to export
 */
final class ToRbt(val snapshot: Snap):
  /**
   * export to RBT
   * @param graph to export
   */
  def this(graph: Root) = this(graph.snap)
  private var nextHandle : Int = 1
  private var handles:Map[CeNode,Int] = Map()
  private def handle( node:CeNode):Int =
    if  ! handles.contains( node ) then
      handles = handles + (node->nextHandle)
      nextHandle = nextHandle + 1
    handles( node)

  def title: String = snapshot.title
  
  def dict: Map[CeNode, String] = snapshot.dict
  
  val sequence: List[CeNode] = snapshot.sequence
  
  def pos( node:CeNode) : Option[Pos] = snapshot.pos(node)

  def mkObs( expr:Expr):String= expr match{
    case _:CSE => ToRbt.obs2Str( Obs.Observable)
    case _ => if snapshot.obs.contains(expr) then ToRbt.obs2Str( snapshot.obs(expr))
    else ToRbt.obs2Str(Obs.Normal)
  }
  
  def mkXY( node:CeNode, xPro:Int, yPro:Int): NodeBuffer =
    val (x,y) = pos(node) match
      case None => (xPro, yPro)
      case Some(pos) => (pos.x,pos.y)

    <X>{x}</X>
    <Y>{y}</Y>

  def mkUi( expr:Expr):String= if snapshot.ui.contains(expr) then snapshot.ui(expr) else "None"

  val ns: Seq[CSE] = sequence.collect { case t: CSE => t }
  
  val es: Seq[Unary|Multi] = sequence.filterNot( _.isInstanceOf[NOT]).collect { case t: Unary => t; case t:Multi=>t }
  
  val cs: Seq[Constraint] = sequence.collect { case t: Constraint => t }
  
  val ts: Seq[NOTE] = sequence.collect { case t: NOTE => t }

  def target( e:Expr ):Expr = e.logic()._1
  
  def mapBoolean( state:Boolean) :String = if state then "T" else "F"
  
  def state( e:Expr, inverted: Boolean=false):String = inverted match{
    case false => mapBoolean(e.logic(inverted)._2)
    case true => mapBoolean(! e.logic(inverted)._2)
  }

  def mkStates(es:Set[Expr]): Set[NodeBuffer] = 
    def mkHandle(ex:Expr)=  s"${handle(ex)}"
    for( e <- es) yield {
      val tar = target(e)
      val sta = state(e)
      <Node>{dict(tar)}</Node>
      <State Handle = {mkHandle(tar)}>{sta}</State>
    }

  private var y = 0
  def toNode( cause:CSE):NodeSeq =
    y = y+1
    val x = 2
    <Node>
      <Handle>{handle(cause)}</Handle>
      <Name>{snapshot.dict(cause)}</Name>
      <TrueDescription>{cause.truthy}</TrueDescription>
      <FalseDescription>{cause.falsy}</FalseDescription>
      <UiType>{mkUi(cause)}</UiType>
      <cObservability>O</cObservability>
      {mkXY(cause,x*150 - 80, y*40)}
      <sNodeType>Primary Cause</sNodeType>
    </Node>
  end toNode
  
  def toNode(expr:Expr):NodeSeq =
    y = y+1
    val x = expr.rank +1
    <Node>
      <Handle>{handle(expr)}</Handle>
      <Name>{snapshot.dict(expr)}</Name>
      <TrueDescription>{expr.truthy}</TrueDescription>
      <FalseDescription>{expr.falsy}</FalseDescription>
      <UiType>{mkUi(expr)}</UiType>
      <cObservability>{mkObs(expr)}</cObservability>
      {mkXY(expr,x*150 - 80, y*40+20)}
      <sNodeType>Primary Effect</sNodeType>
    </Node>
  end toNode
  
  def toConstraint(constraint: Constraint):NodeSeq =
    y = y+1
    val x = 1
    val typ = constraint match
      case x:EXCL => "E"
      case x:INCL => "I"
      case x:ONE => "O"
      case x:ANCHOR => "A"
      case x:REQ => "R"
      case x:MASK =>"M"

    def sub(s:Expr)=
      <SubjectNode>{dict(target(s))}</SubjectNode>
      <Subject>{handle(target(s))}</Subject>
      <cSubjectState>{state(s)}</cSubjectState>

    def mkSubject = constraint match
      case x:EXCL => <Subject></Subject><cSubjectState>X</cSubjectState>
      case x:INCL => <Subject></Subject><cSubjectState>X</cSubjectState>
      case x:ONE => <Subject></Subject><cSubjectState>X</cSubjectState>
      case x:ANCHOR => sub(x.expr)
      case x:REQ => sub(x.src)
      case x:MASK => sub(x.src)

    def mkConstraintStates = constraint.match
      case x:EXCL => mkStates( x.exprs)
      case x:INCL => mkStates( x.exprs)
      case x:ONE => mkStates( x.exprs)
      case x:ANCHOR =>
      case x:REQ => mkStates(x.dests)
      case x:MASK => mkStates(x.dests)

    <Constraint>
      <Handle>{handle(constraint)}</Handle>
      <cConstraintType>{typ}</cConstraintType>
      {mkSubject}
      {mkXY(constraint,x*150 - 80, y*40+20)}
      <States>
        {mkConstraintStates}
      </States>
    </Constraint>
  end toConstraint

  def toRelation(expr:Expr):NodeSeq =
    def mkHandle(ex:Expr) =  s"${handle(ex)}"

    def mkRelationStates = expr match
        case x: EQ => mkStates(Set(x.expr))
        case x: NEQ => mkStates(Set(x.expr))
        case _ => expr.exprs.map{ e =>
          <Node>{dict(target(e))}</Node>
          <State Handle = {mkHandle(target(e))}>{state(e)}</State>
        }

    val symbolRbt = expr match
      case x:EQ => "$"
      case x:NEQ => "$"
      case x:AND => "&"
      case x:OR => "|"
      case x:XOR => "%"
      case x:NXOR =>"^"
      case x:NOR =>"#"
      case x:NAND =>"@"
      case _ => throw IllegalArgumentException( s"expected Relation found $expr")

    <Relation>
      <EffectNode>{dict(expr)}</EffectNode>
      <Handle>{handle(expr)}</Handle>
      <Effect>{handle(expr)}</Effect>
      <cOperator>{symbolRbt}</cOperator>
      <cActiveOrPassive>A</cActiveOrPassive>
      <States>
        {mkRelationStates}
      </States>
    </Relation>
  end toRelation
  
  def toNote( note:NOTE):NodeSeq =
    def mkLinkedConstraints:NodeSeq = {
      val links=note.targets.collect{ case t: Constraint => t }
      def mkCount = links.size.toString

      def makeHandle( n:Constraint): NodeSeq = <ConstraintHandle>{handle(n)}</ConstraintHandle>
      val res = <LinkedConstraints Count={mkCount}>
        {for( r <- links) yield makeHandle(r)}
      </LinkedConstraints>
      res
    }

    def mkLinkedRelations:NodeSeq = {
      val links=note.targets.collect{ case t: Expr => t }
      def mkCount = links.size.toString

      def makeHandle( n:Expr): NodeSeq =
        <Node>{dict(n)}</Node>
        <NodeHandle>{handle(n)}</NodeHandle>

      val res = <LinkedNodes Count={mkCount}>
        {for( r <- links) yield makeHandle(r)}
      </LinkedNodes>
      res
    }

    y=y+1
    <Note>
      <Handle>{handle(note)}</Handle>
      <Text>{note.text}</Text>
      <X>900</X>
      <Y>{y*40}</Y>
      {mkLinkedRelations}
      {mkLinkedConstraints}
    </Note>
  end toNote
  
  def toXML: Elem = 
    sequence.foreach { node =>
      val name = dict(node)
      node match
        case x: CSE =>
        case x: EQ =>
        case x: NEQ =>
        case x: NOT =>
        case x: AND =>
        case x: OR =>
        case x: XOR =>
        case x: NAND =>
        case x: NOR =>
        case x: NXOR =>
        case x: EXCL =>
        case x: INCL =>
        case x: ONE =>
        case x: ANCHOR =>
        case x: MASK =>
        case x: REQ =>
        case x: NOTE =>
        case x: Unary => // should never haben
        case x: Multi =>  // should never haben
    }

    val t = if (title.trim.isEmpty) "generated" else title

    <Document name="RBT">
      <RelationLogicIsCircular>False</RelationLogicIsCircular>
      <Title>
        <TitleString>{t}</TitleString>
        <Author>RBTg</Author>
        <CreationDate></CreationDate>
        <Revision></Revision>
        <Comment></Comment>
      </Title>
      <Nodes>
        {ns.map( toNode)}
        {y=0;es.map(toNode)}
      </Nodes>
      <Constraints>
        {y=0;cs.map(toConstraint)}
      </Constraints>
      <Relations>
        {es.filterNot( _.isInstanceOf[NOT]).map(toRelation)}
      </Relations>
      <Notes>
        {y=0;ts.map(toNote)}
      </Notes>
    </Document>
  end toXML

  def toXmlString = s"${ToRbt.XmlHeader}\n$toXML"

  def writeRbt( fn:String):Unit =
    val xml = toXmlString
    xml.writeTo( fn )

object ToRbt:
  def obs2Str( obs:Obs): String = obs match
    case Obs.Forced => "F"
    case Obs.Observable => "O"
    case Obs.Normal => "N"

  val XmlHeader = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""

extension (ceg: Root)
  inline def writeRbt(fn:String): Unit = ToRbt(ceg).writeRbt(fn)
