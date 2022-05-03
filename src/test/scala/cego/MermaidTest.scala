package cego

import cego.rbtX.{ToRbt, FromRbt}
import cego.graphX.ToMermaid
import cego.scalaX.*

class MermaidTest extends RBTSpec:

  it should "small" in {
    val imp = FromRbt.load(s"$TST_RES/small.rbt")
    val snap = imp.snap
    ToRbt(snap).toXML
    val xml = ToRbt(snap).toXmlString
    xml.writeTo(s"$TARGET/Mermaid.rbt")
    Root.restore(snap)
    val code = ToScala(snap).toString
    code.writeTo(s"$TARGET/Mermaid.txt")
    val gv = ToMermaid(snap)
    println( "-"*50)
    println(gv.toAdoc)
    println( "-"*50)
    println(gv.mkAdocPic)
    val adoc:String=s"""${gv.mkAdocPic}
       |${gv.toAdoc}
       |""".stripMargin
    adoc.writeTo(s"$TARGET/Mermaid.adoc")
    println( "variations".hint)
    imp.Vs.foreach{ v => println (v ) }

    println( "new test run variations".hint)
    imp.Xs.foreach{ v => println (v ) }

    println( "new test run tests".hint)
    imp.Ts.foreach{ t => println (t ) }

    println( "messages".hint)
    imp.Ms.foreach{ m => println (m ) }
  }

