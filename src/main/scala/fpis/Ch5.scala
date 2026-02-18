package fpis

import fpis.Ch5.LazyList.{Cons, cons, empty}

import scala.List

object Ch5 extends App {

  enum LazyList[+A]:
    case Empty
    case Cons(h: () => A, t: () => LazyList[A])

    // 5.1
    def toList: List[A] = this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

    // 5.2
    def take(n: Int): LazyList[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      // special case to avoid unnecessarily compute t()
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _                    => empty
    }

    def drop(n: Int): LazyList[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

    // 5,3
    def takeWhile(f: A => Boolean): LazyList[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case _                    => empty
    }

    def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
      case Empty      => acc
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, acc) => p(a) || acc)

    // 5.4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

    // 5.5
    def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
      foldRight[LazyList[A]](empty)((a, acc) => if p(a) then cons(a, acc) else empty)

    // 5.6
    def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

    // 5.7
    def map[B](f: A => B): LazyList[B] = foldRight[LazyList[B]](empty)((a, b) => cons(f(a), b))

    def filter(p: A => Boolean): LazyList[A] =
      foldRight[LazyList[A]](empty)((a, b) => if p(a) then cons(a, b) else b)

    def append[B >: A](other: LazyList[B]): LazyList[B] = foldRight(other)((a, b) => cons(a, b))

    def flatMap[B](f: (a: A) => LazyList[B]): LazyList[B] =
      foldRight[LazyList[B]](empty)((a, b) => f(a).append(b))

    // 5.13
    def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this):
      case Cons(h, t) => Some(f(h()), t())
      case Empty      => None

    def takeViaUnfold(n: Int): LazyList[A] = unfold((this, n)):
      case (Cons(h, t), 1)          => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _                        => None

    def takeWhileViaUnfold(f: A => Boolean): LazyList[A] = unfold(this):
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _                    => None

    def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)):
      case (Cons(h, t), Empty)          => Some(Some(h()) -> None, t() -> empty)
      case (Empty, Cons(h, t))          => Some((None, Some(h())), (empty, t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(Some(h1()) -> Some(h2()), t1() -> t2())
      case _                            => None

    def startsWith[B >: A](prefix: LazyList[B]): Boolean =
      this.zipAll(prefix).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

  object LazyList:
    def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A] = Empty

    def apply[A](as: A*): LazyList[A] = {
      if as.isEmpty then empty
      else cons(as.head, apply(as.tail*))
    }

  // 5.8
  def continually[A](a: A): LazyList[A] =
    lazy val single = cons(a, continually(a))
    single

  // 5.9
  def from(n: Int): LazyList[Int] =
    lazy val inc = cons(n, from(n + 1))
    inc

  // 5.10
  def fibs: LazyList[Int] = {
    def go(cur: Int, next: Int): LazyList[Int] =
      cons(cur, go(next, cur + next))
    go(0, 1)
  }

  // 5.11
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match {
    case None         => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // 5.12
  def fibsViaUnfold: LazyList[Int] = unfold((0, 1))((cur, next) => Some((cur, (next, cur + next))))
  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(x => Some((x, x + 1)))
  def continuallyViaFold[A](a: A): LazyList[A] = unfold(a)(x => Some((x, x)))

  // 5.1
  println(LazyList(1, 2, 3).toList)
  // 5.2
  println(LazyList(1, 2, 3).take(2).toList)
  println(LazyList(1).take(1).toList)
  println(LazyList(1, 2, 3).drop(1).toList)
  println(LazyList(1, 2, 3).foldRight(0)(_ + _))
  println(LazyList(1, 2, 3).foldRight("")(_ + _))
  println(LazyList(1, 2, 3).takeWhile(_ < 3).toList)
  println(LazyList(1, 2, 3).exists(_ == 31))
  println(LazyList(1, 2, 3).forAll(_ > 1))
  println(LazyList(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList)
  // 5.6
  println(LazyList(1, 2, 3).headOption)
  println(LazyList().headOption)
  // 5.7
  println(LazyList(1, 2, 3).map(_ * 10).toList)
  println(LazyList(1, 2, 3).filter(_ % 2 == 1).toList)
  println(LazyList(1, 2, 3).append(LazyList(4, 5)).toList)
  println(LazyList(1, 2, 3).flatMap(x => LazyList(x, x + 10)).toList)
  // 5.8
  println(continually(1).take(4).toList)
  // 5.9
  println(from(101).take(6).toList)
  // 5.10
  println(fibs.take(10).toList)
  // 5.11
  println(unfold(0)(s => Some((s, s + 2))).take(10).toList)
  // 5.12
  println(unfold((0, 1))((cur, next) => Some((cur, (next, cur + next)))).take(10).toList)
  println(fibsViaUnfold.take(10).toList)
  println(fromViaUnfold(10).take(10).toList)
  println(continuallyViaFold(9).take(10).toList)
  // 5.13
  println(LazyList(1, 2, 3).zipAll(LazyList(4, 5, 6, 7)).toList)
  // 5.14
  println(LazyList(1, 2, 3).startsWith(LazyList(1, 2)))
  println(LazyList(1, 2, 3).startsWith(LazyList(1, 3)))

}
