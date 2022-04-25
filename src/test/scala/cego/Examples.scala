package cego

import scala.annotation.nowarn

trait InRange(name: String, x: Int = 0, y: Int = 0) extends CEG :
  val hi: CSE = cse(s"${name}_hi", s"value $name is > MAX" )
  val lo: CSE = cse(s"${name}_lo", s"value $name is < MIN" )
  val ex: EXCL = excl(hi, lo)

  val in: AND = and( ~hi, ~lo)(s"$name is in", s"$name is valid" )
//  val ms:CSE = cse(s"${name}_ms",s" $name is invalid" )

//  val r1:REQ = req( ms, ~in )
//  val r2:REQ = req( ~ms, in )
//  val ok: AND = and(in, ~ms)(s"${name}_ok", s"$name is ok")
  def rel(n: CeNode, dy: Int): Unit = grid(n, y + dy)

  rel(hi, 0+1)
  rel(ex, 2)
  rel(lo, 2+1)
  rel(in, 1+1)

class InRange_(name: String, x: Int = 0, y: Int = 0)(implicit ceg: CEG) extends Child with InRange(name,x,y)

trait TwoRanges(name: String, x: Int = 0, y: Int = 0) extends CEG :
  val a: InRange = InRange_(s"${name}_A", x, y)(this)
  val b: InRange = InRange_(s"${name}_B", x, y + 6)(this)
  val sum: InRange = InRange_(s"${name}_∑", x, y + 12)(this)

  val ok: AND = and(a.in, b.in, sum.in)(s"${name}_ok", s"$name signal is ok")
  val aMax: CSE = cse(s"${name}_A_max", s"signal $name to high $name" )
  val aNul: CSE = cse(s"${name}_A_null",s"signal $name to low $name" )
  val ex: EXCL = excl(aMax, aNul)

  val scale: AND = and(~aMax, ~aNul)(s"${name}_scale",s"$name active" )
  val max: EQ = eq(aMax)(s"${name}_full", s"$name full")
  val nul: EQ = eq(aNul)(s"${name}_halt",s"$name halt" )
  val msk: MASK = mask(~ok, scale, max, nul)

  def rel(n: CeNode, dy: Int):Unit = grid(n, y + dy)

  rel(ok, 10+1)
  rel(aMax, 18+2)
  rel(aNul, 20+2)
  rel(ex, 19+2)
  rel(max, 17+2)
  rel(scale, 19+2)
  rel(nul, 21+2)
  grid(msk, 3, y + 19+2)

class TwoRanges_(name: String, x: Int = 0, y: Int = 0)(implicit ceg: CEG) extends Child with TwoRanges(name,x,y)

object InRangeInherited extends Root("Range") with InRange("InRange inherited")

object TwoRangesInherited extends Root("Two Ranges") with TwoRanges("TwoRanges inherited")

object TwoRangesDuplicated extends Root("Two ranges duplicated") :

  val one: TwoRanges = TwoRanges_("ONE")(this)
  val two: TwoRanges = TwoRanges_("TWO", 0, 28)(this)
  val both: AND = and(one.ok, two.ok)("Both", "all working")
  val mskOne: MASK = mask(~both, one.scale, one.max, one.nul)
  val mskTwo: MASK = mask(~both, two.scale, two.max, two.nul)

  grid(mskOne, 3, 24)
  grid(both, 34)
  grid(mskTwo, 3, 52)


object Sandbox extends Root("sandbox") :
  val a::b::c::_ =ones( "A", "B", "C")
  add("Logic", a & b | c)
  val dddd = new Child() with InRange("Kanal X")
  add(CSE("Empty"))
  add(CSE("Empty"))
  val x1 = cse("x", "")
  val x2 = cse("y", "Y is true", "Y is false")
  val x3 = cse("z", "")
  add(~x1)
  and(x1, x2)
  and(x1, x2)("", "")
  or(x1, x2)("", "")
//  ones("forward", "backward"->"this goes backward", "neutral")
//  excls("a", "b", "c")
//  ones("a", "b", "c")
//  and(x1, x2, x3)("and with a name", "")
//  add(MASK(x1, x2, x3))
//  add(REQ(x1, x2, x3))
//  add("Öffne €", CSE("10 Euro"))




