package cego

trait MixIn:
  /**
   * title for this
   * @return
   */
  def title: String

  /**
   * nodes => name
   * @return
   */
  def dict: Map[CeNode, String]

  /**
   * order of nodes creation
   * @return
   */
  def sequence: List[CeNode]

  /**
   * position for a node if any
   * @param node
   * @return
   */
  def pos(node: CeNode): Option[Pos]

  /**
   *
   * @param node
   * @return true if node is element
   */
  final def contains(node: CeNode): Boolean = dict.contains(node)

  /**
   * get all Causes (CSE)
   * @return
   */
  final def causes: Set[CSE] = nodes.collect { case t: CSE => t }

  /**
   * get all Expressions (EQ,AND,OR...)
   * @return
   */
  final def expressions: Set[Expr] = nodes.collect { case t: Expr => t }

  /**
   * get all Constraints (ONE,INCL...)
   * @return
   */
  final def constraints: Set[Constraint] = nodes.collect { case t: Constraint => t }

  /**
   * get all Nodes
   * @return
   */
  final def nodes: Set[CeNode] = dict.keySet

  /**
   * get all Notes
   * @return
   */
  final def notes: Set[NOTE] = nodes.collect { case t: NOTE => t }

  /**
   * get all nodes being involved in definition of behavioe ( ! NOTE )
   * @return
   */
  final def logic: Set[Expr | Constraint] = nodes.collect { case t: Expr => t; case t: Constraint => t }
