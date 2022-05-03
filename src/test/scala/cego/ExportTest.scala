package cego

import cego.erpelX.ToErpel

import scala.annotation.nowarn
import cego.rbtX.*
import cego.erpelX.*
import cego.graphX.*
import cego.scalaX.*

class ExportTest extends RBTSpec {
  it should "short" in {
    TwoRangesInherited.snap.dict.keySet.foreach(x =>
      println( TwoRangesInherited.snap.short(x) )
    )
  }

  it should "play" in {
    Sandbox.toScala
    Sandbox.writeRbt(s"$TARGET/Play.rbt")
  }

  it should "erpel" in {
    TwoRangesInherited.mkErpel
    TwoRangesDuplicated.mkErpel
    InRangeInherited.mkErpel
    TwoRangesInherited.mkErpel
  }

  it should "combine" in {
    TwoRangesInherited.writeRbt(s"$TARGET/TwoRangesInherited-gen.rbt")
    InRangeInherited.writeRbt(s"$TARGET/InRangeInherited-gen.rbt")
    TwoRangesDuplicated.writeRbt(s"$TARGET/TwoRangesDuplicated-gen.rbt")
  }

  it should "convenient" in {
    @nowarn
    val g = new Root with Slashed{
      val m =ones(Map( "A"-> "is A", "B"-> "is B"))
      val x::y::_ = ones(List( "is X", "is Y"))
      \|(x,y, m("A"), m("B"))
      val i1 = \("aktiv", "is activ")
      val i2 = \("aktiv2", "is activ 2", "\b")
      val i3 = \("aktiv3", "is activ 3", "is inactiv")
      val a1 = \&(i1,i2,i3)
      val o1 = eq(a1)( "Ok1", "Good")
      val o2 = eq(a1)( "Ok2", "Better")
      val o3 = eq(a1)( "Ok3", "Legend")
      val o4 = eq(a1)
      val xxx = or(i1,i2,i3)("xxx", "kllklklk", "ioioioioo")

      val hv=cse(valid("xxx", "XXXXXXX"))
      val he=cse(valid("yyy", "YYYYYYY"))
      val hg=nor(hv,he)(valid("zzz", "ZZZZZZ"))
      \<*<("LTGT","5", "6")
      \<*<=("LTGE","5", "6", "scales", "null","max" )
      \<=*<("LEGT","5", "6", "bad"->"is bad")
      \<=*<=("LEGE","5", "6", "ugly")
    }

    g.writeRbt( s"$TARGET/Convenient.rbt" )

  }

  it should "small" in {
    val snap = FromRbt.load(s"$TST_RES/small.rbt").snap
    ToRbt(snap).toXML
    val xml = ToRbt(snap).toXmlString
    xml.writeTo( s"$TARGET/Small.rbt" )
    Root.restore(snap)
    val code = ToScala(snap).toString
    code.writeTo(s"$TARGET/Small.txt")
    val gv = ToGraphviz(snap)
    println(gv.toAdoc)
    println( "-"*50)
    println(gv.mkDrawingCommands.mkString("\n"))
  }

  it should "erpel 2" in {
    val s = new Root("abc") with Slashed {
      val a = cse( "a", "One"  )
      val b = cse( "b", "Two"  )
      val x = or( a,b)( "good", "the result is good" )
      val y = and( a,b)
      val z = or(x,y)
      setUi(a, "Button")
      setObs( y,Obs.Observable)
    }

    ToErpel(s.snap)
    println( ToScala(s.snap) )
    s.writeRbt(s"$TARGET/erpel-2.rbt")
  }
}

