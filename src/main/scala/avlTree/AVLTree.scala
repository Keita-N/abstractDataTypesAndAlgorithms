package avlTree

/**
 * AVL Tree
 */
object Tree {
  def apply(xs: Seq[Int]) = create(xs)
  def create(xs: Seq[Int]): Tree = {
    xs.foldLeft[Tree](Empty)((t, i) => t.avl_insert(i))
  }
}
sealed trait Tree {
  val balance: Balance
  def _element: Option[Int] = this match {
    case Branch(e, _, _) => Some(e)
    case Node(e) => Some(e)
    case Empty => None
  }
  def _left = this match {
    case Branch(e, l, r) => l
    case _ => Empty
  }

  def _right = this match {
    case Branch(_, _, r) => r
    case _ => Empty
  }

  def exists(elem: Int): Boolean = this match {
    case Node(e) => elem == e
    case Branch(e, l, r) =>
      if (elem == e) true
      else if (elem < e) l.exists(elem)
      else r.exists(elem)
    case Empty => false
  }

  def fold[A](a: A)(f: (A, Int) => A): A = this match {
    case Branch(e, l, r) =>
      r.fold(f(l.fold(a)(f), e))(f)
    case Node(e) => f(a, e)
    case Empty => a
  }

  def forAll(f: Int => Boolean): Boolean = fold(true)(_ && f(_))

  /**
   * 通りがけ順
   * @param f
   */
  def in_order(f: Int => Unit): Unit = this match {
    case Branch(e, l, r) =>
      l.in_order(f)
      f(e)
      r.in_order(f)
    case Node(e) => f(e)
    case Empty => // do nothing
  }

  /**
   * 行きがけ順
   * @return
   */
  def pre_order(f: Int => Unit): Unit = this match {
    case Branch(e, l, r) =>
      f(e)
      l.pre_order(f)
      r.pre_order(f)
    case Node(e) => f(e)
    case Empty =>
  }

  /**
   * 帰りがけ順
   * @return
   */
  def post_order(f: Int => Unit): Unit = this match {
    case Branch(e, l, r) =>
      l.post_order(f)
      r.post_order(f)
      f(e)
    case Node(e) => f(e)
    case Empty =>
  }

  def depth: Int = this match {
    case Node(e) => 1
    case Branch(e, l, r) => 1 + Math.max(l.depth, r.depth)
    case Empty => 0;
  }

  def left_depth: Int = this match {
    case Branch(_, l, _) => l.depth
    case _ => 0
  }

  def right_depth: Int = this match {
    case Branch(_, _, r) => r.depth
    case _ => 0
  }

  def binary_insert(elem: Int): Tree = this match {
    case Node(e) =>
      if (elem == e) this
      else if (elem < e) Branch(e,Node(elem), Empty)
      else Branch(e, Empty, Node(elem))
    case Branch(e, l, r) =>
      if (elem == e) this
      else if (elem < e) Branch(e, l.binary_insert(elem), r)
      else Branch(e, l, r.binary_insert(elem))
    case Empty => Node(elem)
  }

  def avl_insert(elem: Int): Tree = {
    val (t, _) = _avl_insert_(this)(elem)
    t
  }

  /**
   * AVL平衡になるように要素を挿入する。
   * 要素を追加した後の木と、木の高さが増加したかを返す
   *
   * @param tree
   * @param elem
   * @return
   */
  private def _avl_insert_(tree: Tree)(elem: Int): (Tree, Boolean) =
    tree match {
      case Empty => (Node(elem), true)
      case Node(e) =>
        if (elem == e) (tree, false)
        else if (elem < e) (Branch(e, Node(elem), Empty), true)
        else (Branch(e, Empty, Node(elem)), true)
        // 元の木の(要素、左部分木、右部分木)
      case Branch(e, l, r) =>
        if (elem == e) (tree, false)
        else if (elem < e) {
          // 左部分木に挿入
          val (avl_lt, increased) = _avl_insert_(l)(elem)
          (increased, tree.balance, avl_lt.balance) match {
            case (false, _, _) => (Branch(e, avl_lt, r), false)
            case (true, RightHigher, _) => (Branch(e, avl_lt, r), false)
            case (true, Balanced, _) => (Branch(e, avl_lt, r), increased)
            case (true, LeftHigher, LeftHigher) =>
              (Branch(
                avl_lt._element.get,
                avl_lt._left,
                Branch(e, avl_lt._right, r)
              ), false)
            case (true, LeftHigher, RightHigher) =>
              // AVL平衡した左部分木の右部分木
              val lr_tree = avl_lt._right
              val (lr_e, lr_l, lr_r) =
                (lr_tree._element, lr_tree._left, lr_tree._right)
              (Branch(lr_e.get,
                Branch(avl_lt._element.get, avl_lt._left, lr_l),
                Branch(e, lr_r, r)), false)
            case (true, LeftHigher, Balanced) =>
              (Branch(e, avl_lt, r), false)
          }
        } else {
          // 右部分木に挿入
          val (avl_rt, increased) = _avl_insert_(r)(elem)
          (increased, tree.balance, avl_rt.balance) match {
            case (false, _, _) => (Branch(e, l, avl_rt), false)
            case (true, LeftHigher, _) => (Branch(e, l, avl_rt), false)
            case (true, Balanced, _) => (Branch(e, l, avl_rt), increased)
            case (true, RightHigher, RightHigher) =>
              val (r_e, r_l, r_r) =
                (avl_rt._element, avl_rt._left, avl_rt._right)
              (Branch(r_e.get,
                Branch(e, l, r_l),
                r_r), false)
            case (true, RightHigher, LeftHigher) =>
              val r_l = avl_rt._left
              // AVL平衡した右部分木の左部分木
              val (rl_e, rl_l, rl_r) =
                (r_l._element, r_l._left, r_l._right)
              (Branch(rl_e.get,
                Branch(e, l, rl_l),
                Branch(avl_rt._element.get, rl_r, avl_rt._right)),
                false)
            case (true, RightHigher, Balanced) =>
              (Branch(e, l, avl_rt), false)
          }
        }
  }
}
case class Node(elem: Int) extends Tree {
  val balance = Balanced
}
case class Branch(
  elem: Int,
  private val l: Tree,
  private val r: Tree) extends Tree {
  val left = toNode(l)
  val right = toNode(r)

  val balance = {
    val l_dep = left.depth
    val r_dep = right.depth
    if (l_dep == r_dep) Balanced
    else if (l_dep < r_dep) RightHigher
    else LeftHigher
  }

  private def toNode(tree: Tree): Tree = tree match {
    case Branch(e, Empty, Empty) => Node(e)
    case _ => tree
  }

  override def toString =
    s"Branch($elem, $left, $right)"

}

case object Empty extends Tree {
  val balance = Balanced
}

sealed trait Balance
case object Balanced extends Balance
case object LeftHigher extends Balance
case object RightHigher extends Balance

