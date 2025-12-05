package scalawithcats

object Ex7_2_Monoid extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }

  given booleanAndMonoid: Monoid[Boolean] with {
    def combine(a: Boolean, b: Boolean): Boolean = a && b

    def empty = true
  }

  given setMonoid[A]: Monoid[Set[A]] with {
    def combine(a: Set[A], b: Set[A]): Set[A] = a ++ b
    def empty: Set[A] = Set.empty
  }

  println(Monoid[Boolean].combine(true, true))
  println(Monoid[Boolean].empty)

  println(Monoid[Set[Int]].combine(Set(1, 2), Set(2, 3)))
}
