package fpis

import fpis.Ch5.LazyList.{cons, empty}
import fpis.Ch5.LazyList.Cons
import fpis.Ch5.LazyList.Empty

import scala.List
import scala.annotation.tailrec

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
      foldRight[LazyList[A]](empty)((a, acc) => if p(a) then cons(a, acc) else acc)

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

}
