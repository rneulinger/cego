package cego

type Range = (CSE, CSE, AND|OR, EXCL, Option[EQ], Option[EQ])

/**
 * Convenience Methods with (Back)slash syntax
 */
trait Slashed:
  this: CEG =>

  /**
   * create a named cause
   *
   * @param name   of node
   * @param truthy true description. if empty name is used.
   * @param falsy  false description.
   * @return
   * if node with this name exists
   */
  final def \(name: String, truthy: String = "", falsy: String = ""): CSE = cse(name, truthy, falsy)

  /** create anonymous and */

  final def \&(e1: Expr, e2: Expr, more: Expr*): AND = and(e1, e2, more *)

  final def \&(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String = "", falsy: String = ""): AND =
    and(e1, e2, more *)(name, truthy, falsy)

  final def \&(n: Int): AND = and(n)

  /** create anonymous or */
  final def \|(e1: Expr, e2: Expr, more: Expr*): OR = or(e1, e2, more *)

  final def \|(e1: Expr, e2: Expr, more: Expr*)(name: String, truthy: String = "", falsy: String = ""): OR =
    or(e1, e2, more *)(name, truthy, falsy)

  /**
   * add relation
   * @param op relational operator >, >= ...
   * @param name of value
   * @param ref to compare to
   * @param style how to create Node
   * @return
   */
  private def addRelCse(op: RelOp, name: String, ref: String, style: Style): CSE =
    val truthy = s"$name is ${op.desc.truthy} $ref"
    val res = style match
      case Style.Explicit => CSE(truthy, s"$name is ${op.desc.falsy} $ref")
      case Style.Muted => CSE(truthy, Muted)
      case Style.Negated => CSE(truthy, Negated)

    add(s"$name ${op.desc.truthy}$ref", res)
    res

  /**
   * x > ref [..]
   *
   * @param name of value
   * @param ref  to compare to
   * @return
   */
  final def \>(name: String, ref: String, style: Style = Explicit): CSE = addRelCse(RelOp.GT, name, ref, style)

  /**
   * x >= ref [..]
   *
   * @param name of value
   * @param ref  to compare to
   * @return
   */
  final def \>=(name: String, ref: String, style: Style = Explicit): CSE = addRelCse(RelOp.GE, name, ref, style)

  /**
   * x == ref [..]
   *
   * @param name of value
   * @param ref  to compare to
   * @return
   */
  final def \=(name: String, ref: String, style: Style = Explicit): CSE = addRelCse(RelOp.EQ, name, ref, style)

  /**
   * x != ref [..]
   *
   * @param name of value
   * @param ref  to compare to
   * @return
   */
  final def \!(name: String, ref: String, style: Style = Explicit): CSE = addRelCse(RelOp.NE, name, ref, style)

  /**
   * x <= ref [..]
   *
   * @param name of value
   * @param ref  to compare to
   * @return
   */
  final def \<=(name: String, ref: String, style: Style = Explicit): CSE = addRelCse(RelOp.LE, name, ref, style)

  /**
   * x < ref [..]
   *
   * @param name of value
   * @param ref  to compare to
   * @return
   */
  final def \<(name: String, ref: String, style: Style = Explicit): CSE = addRelCse(RelOp.LT, name, ref, style)

  /**
   *
   * @param name name
   * @param hi   upper bound definition
   * @param lo   lower bound definition
   * @return
   */
  private def mkRange(name: String, hi: CSE, lo: CSE, desc:Desc, high:Desc = "", low:Desc = "")(out:Boolean=false): Range =

    val inf = toInf(desc) match
      case Ano =>
        if out then Inf( name + "_out", name + " is out of range", name + " is within range")
        else Inf( name + "_in", name + " is within range", name + " is out of range")
      case x => x

    val infHi = toInf(high) match
      case Ano => Ano
      case Inf(x,Empty) => Inf( x, name + " is too big")
      case x => x

    val infLo = toInf(low) match
      case Ano => Ano
      case Inf(x,Empty) => Inf( x, name + " is too small")
      case x => x

    // create hi first because of vertical alignment in layout
    val _hi = if infHi == Ano then None else Some(eq(hi, infHi))

    val _rng:(AND|OR) = out match
      case false => and (~ lo, ~ hi) (inf)
      case true => or (lo, hi) (inf)

    val _lo = if infLo == Ano then None else Some(eq(lo, infLo))

    (lo, hi, _rng, excl(lo, hi),_lo,_hi)


  /**
   * a < x < b ]x[
   *
   * @return
   */
  final def \<*<(name: String, lo: String, hi: String, in:Desc="", l:Desc="", h:Desc=""): Range =
    mkRange(name, \>(name, hi, Muted), \<(name, lo, Muted), in,h,l)()

  /**
   * a <= x < b [x[
   *
   * @return
   */
  final def \<=*<(name: String, lo: String, hi: String, in:Desc="", h:Desc="", l:Desc=""): Range =
    mkRange(name, \>(name, hi, Muted), \<=(name, lo, Muted), in,h,l)()

  /**
   * a < x <= b  ]x]
   *
   * @return
   */
  final def \<*<=(name: String, lo: String, hi: String, in:Desc="", h:Desc="", l:Desc=""): Range =
    mkRange(name, \>=(name, hi, Muted), \<(name, lo, Muted),  in,h,l)()

  /**
   * a <= x <= b  [x]
   *
   * @return
   */
  final def \<=*<=(name: String, lo: String, hi: String, in:Desc="", h:Desc="", l:Desc=""): Range =
    mkRange(name, \<=(name, hi, Muted), \<=(name, lo), in,h,l)()

  /**
   * x < a < x  [x]
   *
   * @return
   */
  final def \</<(name: String, lo: String, hi: String, out:Desc="", l:Desc="", h:Desc=""): Range =
    mkRange(name, \>(name, hi, Muted), \<(name, lo, Muted), out,h,l)(true)

  /**
   * x <= a < x [x[
   *
   * @return
   */
  final def \<=/<(name: String, lo: String, hi: String, out:Desc="", h:Desc="", l:Desc=""): Range =
    mkRange(name, \>(name, hi, Muted), \<=(name, lo, Muted), out,h,l)(true)

  /**
   * x < a <= x  ]x]
   *
   * @return
   */
  final def \</<=(name: String, lo: String, hi: String, out:Desc="", h:Desc="", l:Desc=""): Range =
    mkRange(name, \>=(name, hi, Muted), \<(name, lo, Muted),  out,h,l)(true)

  /**
   * x <= a <= x  [x]
   *
   * @return
   */
  final def \<=/<=(name: String, lo: String, hi: String, out:Desc="", h:Desc="", l:Desc=""): Range =
    mkRange(name, \<=(name, hi, Muted), \<=(name, lo), out,h,l)(true)
