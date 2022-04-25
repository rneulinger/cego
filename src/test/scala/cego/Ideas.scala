package cego

object Example extends Root with Slashed:
  val nrm: String =
    """   a
   a  """.normalize()

  val cg: Root = Root()
  //cg \>= ("","")
  cg.and( 2 )
  add(CSE())

  def xxxMain(): Unit =
    println("hello")

    val a = cse("a", "")

    val b = \("b")
    val c = \("c")
    val d = \("d")
    val f = \("d")
    println(AND(a, b))
    println(a | b & c & d & d)
    println(a & f & f)
    println(~(~a))
