package scalawithcats


object Ex2 extends App {

  enum Tree[A] {
    case Leaf(a: A)
    case Node(left: Tree[A], right: Tree[A])

    def size: Int = fold(_ => 1, _ + _)

    def contains(el: A): Boolean = fold(_ == el, _ || _)

    def map[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), (l, r) => Node(l, r))

    def fold[B](empty: A => B, f: (B, B) => B): B = this match {
      case Leaf(a) => empty(a)
      case Node(l, r) => f(l.fold(empty, f), r.fold(empty, f))
    }

    def fold2[B](empty: A => B)(f: (B, B) => B): B = this match {
      case Leaf(a) => empty(a)
      case Node(l, r) => f(l.fold(empty, f), r.fold(empty, f))
    }
  }

  import scalawithcats.Ex2.Tree.{Leaf, Node}


  assert(2 == Node(Leaf(1), Leaf(3)).size)
  assert(3 == Node(Leaf(1), Node(Leaf(3), Leaf(3))).size)

  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).contains(2))
  assert(!Node(Leaf(1), Node(Leaf(2), Leaf(3))).contains(4))

  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 10).contains(11))
  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 10).contains(12))
  assert(Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 10).contains(13))

}
