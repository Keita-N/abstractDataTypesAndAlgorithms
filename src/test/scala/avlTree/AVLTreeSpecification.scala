package avlTree

import org.scalacheck.Properties

object AVLTreeSpecification extends Properties("AVLTree") {
  import org.scalacheck.Prop.forAll

  property("挿入した要素がすべて木に含まれる") = forAll {
    (xs: Seq[Int]) => {
      val t = Tree(xs)
      xs.forall(i => t.exists(i))
    }
  }

  property("すべてに左部分木の要素は根より小さい") = forAll {
    (xs: Seq[Int]) => {
      val t = Tree(xs)
      t match {
        case Branch(e, l, r) => l.forAll(_ < e)
        case _ => true
      }
    }
  }

  property("すべてに右部分木の要素は根より大きい") = forAll {
    (xs: Seq[Int]) => {
      val t = Tree(xs)
      t match {
        case Branch(e, l, r) => r.forAll(_ > e)
        case _ => true
      }
    }
  }

  property("左部分木と右部分木の深さの差は最大で１") = forAll {
    (xs: Seq[Int])  => {
      val t = Tree(xs)
      Math.abs(t.left_depth - t.right_depth) <= 1
    }
  }

}
