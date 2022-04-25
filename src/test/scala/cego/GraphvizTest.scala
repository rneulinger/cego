package cego

import cego.rbtX.{ToRbt, FromRbt}
import cego.graphvizX.*
import cego.scalaX.*
class GraphvizTest extends RBTSpec {
  it should "small" in {
    val imp = FromRbt.load(s"$TST_RES/small.rbt")
    val snap = imp.snap
    ToRbt(snap).toXML
    val xml = ToRbt(snap).toXmlString
    xml.writeTo( s"$TARGET/Generated.rbt" )
    Root.restore(snap)
    val code = ToScala(snap).toString
    code.writeTo(s"$TARGET/Generated.txt")
    imp.toAdoc.writeTo(s"$TARGET/Generated.adoc")
  }
}
