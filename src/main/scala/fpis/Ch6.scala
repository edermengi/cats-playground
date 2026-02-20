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
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d, i), r2)
  }

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
}
