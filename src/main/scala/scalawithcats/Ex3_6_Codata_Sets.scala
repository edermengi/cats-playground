package scalawithcats

object Ex3_6_Codata_Sets extends App {

  trait Set[A] {
    def contains(elt: A): Boolean
    def insert(elt: A): Set[A] = InsertOneSet(elt, this)
    def union(that: Set[A]): Set[A] = UnionSet(this, that)
  }

  final class InsertOneSet[A](element: A, source: Set[A]) extends Set[A] {
    def contains(elt: A): Boolean = elt == element || source.contains(elt)
  }

  final class UnionSet[A](first: Set[A], second: Set[A]) extends Set[A] {
    def contains(elt: A): Boolean = first.contains(elt) || second.contains(elt)
  }

  val evens = new Set[Int] {
    override def contains(elt: Int): Boolean = elt % 2 == 0
  }

  final class IndicatorSet[A](indicator: A => Boolean) extends Set[A] {
    def contains(elt: A): Boolean = indicator(elt)
  }

  val odds = IndicatorSet[Int](_ % 2 == 1)

  println(evens.contains(1))
  println(evens.contains(2))
  println(evens.contains(3))

  println(odds.contains(1))
  println(odds.contains(2))
  println(odds.contains(3))


}
