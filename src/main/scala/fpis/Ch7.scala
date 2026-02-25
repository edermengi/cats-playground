package fpis

import scala.concurrent.duration.TimeUnit

object Ch7 extends App {
  class ExecutorService:
    def submit[A](a: Callable[A]): Future[A]

  trait Callable[A]:
    def call: A

  trait Future[A]:
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean

  opaque type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    extension [A](pa: Par[A]) def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = ???

    extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.fork(sum(l)).map2(Par.fork(sum(r)))(_ + _)
  
  println("Hi")
}
