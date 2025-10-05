package scalawithcats

object Ex1 {

  sealed trait Tree[A]

  final case class Leaf[A](a: A) extends Tree

  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree

  enum Tree3[A] {
    case Leaf(a: A)
    case Node(left: Tree3[A], right: Tree3[A])
  }

}
