package scalawithcats

import scalawithcats.Ex1.Tree.{Leaf, Node}

object Ex1 extends App {

  //  sealed trait Tree[A]
  //
  //  final case class Leaf[A](a: A) extends Tree
  //
  //  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree

  enum Tree[A] {
    case Leaf(a: A)
    case Node(left: Tree[A], right: Tree[A])

    def size: Int = this match {
      case Leaf(a)           => 1
      case Node(left, right) => left.size + right.size
    }

    def contains(el: A): Boolean = this match {
      case Leaf(a)           => a == el
      case Node(left, right) => left.contains(el) || right.contains(el)
    }

    def map[B](f: A => B): Tree[B] = this match {
      case Leaf(a)    => Leaf(f(a))
      case Node(l, r) => Node(l.map(f), r.map(f))
    }
  }

  assert(2 == Node(Leaf(1), Leaf(3)).size)
  assert(3 == Node(Leaf(1), Node(Leaf(3), Leaf(3))).size)

  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).contains(2))
  assert(!Node(Leaf(1), Node(Leaf(2), Leaf(3))).contains(4))

  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 10).contains(11))
  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 10).contains(12))
  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 10).contains(13))

}
