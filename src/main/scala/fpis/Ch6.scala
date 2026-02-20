package fpis

import scala.annotation.tailrec
import scala.List
import scala.LazyList

object Ch6 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    if n > 0 then (n, r)
    else (-n + 1, r)
  }

  // 6.2
  def double: Rand[Double] = map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  // 6.3
  def intDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)

  def doubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def ints0(cnt: Int, r: RNG, l: scala.List[Int]): (List[Int], RNG) = {
      if cnt == 0 then (l, r)
      else
        val (i, r2) = r.nextInt
        ints0(cnt - 1, r2, i :: l)
    }
    ints0(count, rng, Nil)
  }

  // 6.5
  private type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, r) = s(rng)
      (f(a), r)

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a, b), rb2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  // 6.7
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      @tailrec
      def seq0(rrs: List[Rand[A]], r: RNG, l: List[A]): (List[A], RNG) =
        rrs match {
          case Nil    => (l, r)
          case h :: t =>
            val (a2, r2) = h(r)
            seq0(t, r2, a2 :: l)
        }
      seq0(rs, rng, Nil)

  def sequenceViaFold[A](rs: List[Rand[A]]): Rand[List[A]] = rng =>
    rs.foldRight((List[A](), rng))((r, s) =>
      val (a, r2) = r(s._2)
      (a :: s._1, r2)
    )

  def sequenceViaFoldAndMap2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  // 6.1
  val r = SimpleRNG(30)
  val l: LazyList[Int] = LazyList.unfold[Int, RNG](r)(rng => Some(nonNegativeInt(rng)))
  println(l.take(10).toList)

  // 6.2
  val l2 = LazyList.unfold[Double, RNG](r)(rng => Some(double(rng)))
  println(l2.take(10).toList)

  // 6.3
  println(
    LazyList
      .unfold[(Int, Double), RNG](r)(rng => Some(intDouble(rng)))
      .take(5)
      .toList
  )
  println(LazyList.unfold[(Double, Int), RNG](r)(rng => Some(doubleInt(rng))).take(5).toList)
  println(
    LazyList
      .unfold[(Double, Double, Double), RNG](r)(rng => Some(double3(rng)))
      .take(5)
      .toList
  )

  // 6.4
  println(ints(5)(r))

  // 6.7
  println(sequence(List.fill(5)(nonNegativeInt))(r))
  println(sequenceViaFold(List.fill(5)(nonNegativeInt))(r))
  println(sequenceViaFoldAndMap2(List.fill(5)(nonNegativeInt))(r))
}
