package fpis

object Ch7 {

  trait Par[A] {}

  object Par {
    def unit[A](a: A): Par[A] = ???
    def get[A](a: Par[A]): A = ???
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sum(l), sum(r))(_ + _)

}
