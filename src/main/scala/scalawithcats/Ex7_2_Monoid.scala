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

  println(Monoid[Boolean].combine(true, true))
  println(Monoid[Boolean].empty)
}
