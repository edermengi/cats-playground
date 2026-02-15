package fpis

import fpis.Ch5.LazyList.{cons, empty}

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

}
