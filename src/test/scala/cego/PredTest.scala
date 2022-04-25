package cego

class PredTest extends RBTSpec {
  it should "create Empty from empty arguments" in {
    assert( Pred(" "," \t") == Empty)
  }
  it should "create Ignore from falsy = '/b" in {
    val p=Pred( "", "/b")
    assert( p.truthy.isEmpty)
    assert( p.falsy == "/b")
    assert( ! p.full)
    assert( ! p.negated)
    assert( p.muted)
    assert( ! p.empty)
  }

  it should "create Ignore from both = '/b" in {
    val p=Pred( "/b", "/b")
    assert( p.truthy.isEmpty)
    assert( p.falsy == "/b")
    assert( ! p.full)
    assert( ! p.negated)
    assert( p.muted)
    assert( ! p.empty)
  }

  it should "create Ignore from falsy = '/B" in {
    val p=Pred( "", "/B")
    assert( p.truthy.isEmpty)
    assert( p.falsy == "/b")
    assert( ! p.full)
    assert( ! p.negated)
    assert( p.muted)
    assert( ! p.empty)
  }

  it should "create Full from two arguments" in {
    val p = Pred("a","b")
    assert( p.full)
    assert( ! p.negated)
    assert( ! p.muted)
    assert( ! p.empty)
  }


  it should "trim arguments" in{
    val p= Pred( " a ", " b ")
    assert(p.truthy == "a")
    assert(p.falsy == "b")
  }
}
