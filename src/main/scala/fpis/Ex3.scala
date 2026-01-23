package fpis

import scala.annotation.tailrec

enum List[+A] {
  case Nil
  case Cons(head: A, tail: List[A])
}

object List {
  def apply[A](xs: A*): List[A] =
    if xs.isEmpty then Nil
    else Cons(xs.head, List(xs.tail*))
}

def tail[A](xs: List[A]): List[A] =
  xs match {
    case List.Nil           => sys.error("")
    case List.Cons(_, tail) => tail
  }

def setHead[A](head: A, xs: List[A]): List[A] =
  xs match {
    case List.Nil           => List(head)
    case List.Cons(_, tail) => List.Cons(head, tail)
  }

@tailrec
def drop[A](as: List[A], n: Int): List[A] = {
  if n == 0 then as
  else
    as match {
      case List.Nil           => List.Nil
      case List.Cons(_, tail) => drop(tail, n - 1)
    }
}

// 3.5
@tailrec
def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
  as match {
    case List.Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _                                => as
  }
}

// 3.6
def init[A](as: List[A]): List[A] = {
  as match {
    case List.Nil | List.Cons(_, List.Nil) => List.Nil
    case List.Cons(h, tail)                => List.Cons(h, init(tail))
  }
}

object Test extends App {
  val l1 = List(1, 2, 3, 4, 5)
  println(l1)
  println(tail(l1))
  println(setHead(10, l1))
  println(drop(l1, 2))
  println(dropWhile(l1, _ < 3))
  println(init(l1))
}
