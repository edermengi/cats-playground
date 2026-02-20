package fpis

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

  // 6.1
  val r = SimpleRNG(30)
  val l: LazyList[Int] = LazyList.unfold[Int, RNG](r)(rng => Some(nonNegativeInt(rng)))
  println(l.take(10).toList)

  // 6.2
  val l2 = LazyList.unfold[Double, RNG](r)(rng => Some(double(rng)))
  println(l2.take(10).toList)

}
