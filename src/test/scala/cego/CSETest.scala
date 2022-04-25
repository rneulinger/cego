package cego

class CSETest extends RBTSpec {
  it should "have a correct default value for empty" in {
    val x = CSE()
    assert(x.truthy.isEmpty)
    assert(x.falsy == "/b")
  }
  it should "reject a cause with missing true description" in {
//    intercept[IllegalArgumentException] {
//      val x = CSE("")
//    }
  }
  it should "trim descriptions" in {
    val x = CSE("a ", " b")
    assert(x.truthy == "a")
    assert(x.falsy == "b")
  }
  it should "normalize descriptions" in {
    val x = CSE("a a", "  b")
    assert(x.truthy == "a a")
    assert(x.falsy == "b")
  }

  /*
  it should "normalize multiline descriptions" in {
    val x = Cause("""   a
   a  """, "  b")
    println(x)
    assert(x.truthy ==
      """a
        |a""".stripMargin)
    assert(x.falsy == "b")
  }
  */
}

