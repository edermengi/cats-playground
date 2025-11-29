package scalawithcats

object Ex3_5_Codata_List extends App {

  trait List[A] {
    def foldRight[B](empty: B)(f: (A, B) => B): B
  }
  final class Pair[A](head: A, tail: List[A]) extends List[A] {
    def foldRight[B](empty: B)(f: (A, B) => B): B = f(head, tail.foldRight(empty)(f))
  }
  final class Empty[A] extends List[A] {
    def foldRight[B](empty: B)(f: (A, B) => B): B = empty
  }
  println()
}
