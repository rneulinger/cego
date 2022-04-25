package cego

import cego.scalaX.*


class CEGTest extends RBTSpec {
  it should "add a node" in {
    object g extends Root with Slashed{
      add( CSE("Empty"))
      add( CSE("Empty"))
      val x1 =  \("x")
      val x2 =  \( "y","this is true", "this is false")
      val x3 =  \( "z")
      add( ~ x1 )
      and(x1,x2)
      or(x1,x2)("2 OR")
      and( x1,x2,x3)("3 AND","Three ANDs", Negated)
      ones( "forward", "backward", "neutral")
      excls( "a", "b", "c")
      ones( "a", "b", "c")
      and(x1,x2,x3)("and with a name", "")
      add( MASK(x1,x2,x3))
      add( REQ(x1,x2,x3))
      add( "Öffenne €", CSE("10 Euro"))

    }
    assert( g.contains(g.x1) )
    println( "*"*50)
    // println(g.snapShot.toScala )
    println( g.snap.effects())
    println( g.snap.filter(OR(g.x1,g.x2)))
    println( g.snap.filter(g.x2) )
    println( g.toScala)
  }
}


