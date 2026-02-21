package fpis

import fpis.Ch6.{RNG, SimpleRNG, sequence}

import scala.List

object Ch6_2 extends App {

  opaque type State[S, +A] = S => (A, S)

  object State:
    extension [S, A](underlying: State[S, A])
      def run(s: S): (A, S) = underlying(s)

      def map[B](f: A => B): State[S, B] = s =>
        val (a, sa) = underlying(s)
        (f(a), sa)

      def flatMap[B](f: A => State[S, B]): State[S, B] = s =>
        val (a, sa) = underlying(s)
        f(a)(sa)

      def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        for {
          a <- underlying
          b <- sb
        } yield f(a, b)

    def apply[S, A](f: S => (A, S)): State[S, A] = f

    def unit[S, A](a: A): State[S, A] = s => (a, s)

    def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
      rs.foldRight(unit(Nil: List[A]))((s, acc) => s.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]

  def nonNegativeInt: Rand[Int] = rng => rng.nextInt

  val r = SimpleRNG(10)

  println(nonNegativeInt(r))
  println(sequence(List.fill(10)(nonNegativeInt))(r))
}
