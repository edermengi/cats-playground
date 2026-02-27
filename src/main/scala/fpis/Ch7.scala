//noinspection ScalaWeakerAccess
package fpis

import fpis.Ch7.Par.map2
import scala.List
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.TimeUnit

object Ch7 extends App {
  abstract class ExecutorService:
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
    def unit[A](a: A): Par[A] = es => UnitFuture(a)
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    private case class UnitFuture[A](get: A) extends Future[A]:
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false

    extension [A](pa: Par[A])
      def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

    extension [A](pa: Par[A])
      def map[B](f: A => B): Par[B] =
        pa.map2(unit(()))((a, _) => f(a))

    extension [A](pa: Par[A])
      def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
        es =>
          new Future[C]:
            private val futureA = pa(es)
            private val futureB = pb(es)
            @volatile private var cache: Option[C] = None

            def isDone: Boolean = cache.isDefined

            def get: C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

            def get(timeout: Long, units: TimeUnit): C =
              val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, units)
              val started = System.nanoTime
              val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
              val elapsed = System.nanoTime - started
              val b = futureB.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
              val c = f(a, b)
              cache = Some(c)
              c

            def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

            def cancel(evenIfRunning: Boolean): Boolean =
              futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

    def fork[A](a: => Par[A]): Par[A] = es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    // 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(Nil): Par[List[A]])((pa, acc) => pa.map2(acc)(_ :: _))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      fork:
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)

    // 7.6
    def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] =
      parMap(ps.filter(f))(a => a)

  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.fork(sum(l)).map2(Par.fork(sum(r)))(_ + _)

  println("Hi")
}
