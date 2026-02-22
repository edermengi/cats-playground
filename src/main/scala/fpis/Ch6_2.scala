package fpis

import fpis.Ch6.{RNG, SimpleRNG, sequence}
import fpis.Ch6_2.Input.{Coin, Turn}
import fpis.Ch6_2.State.{flatMap, get}

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

    def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
      as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

    def get[S]: State[S, S] = s => (s, s)

    def set[S](s: S): State[S, Unit] = _ => ((), s)

    def modify[S](f: S => S): State[S, Unit] =
      for
        s <- get
        _ <- set(f(s))
      yield ()

  type Rand[A] = State[RNG, A]

  def nonNegativeInt: Rand[Int] = rng => rng.nextInt

  val r = SimpleRNG(10)

  println(nonNegativeInt(r))
  println(sequence(List.fill(10)(nonNegativeInt))(r))

  // 6.11
  enum Input:
    case Coin, Turn

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def update(input: Input)(s: Machine): Machine =
    (input, s) match
      case (_, Machine(_, 0, _))                    => s
      case (Input.Coin, Machine(false, _, _))       => s
      case (Input.Turn, Machine(true, _, _))        => s
      case (Input.Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Input.Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- get
    yield (s.coins, s.candies)

  val m = Machine(true, 5, 10)

  println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))(m))
}
