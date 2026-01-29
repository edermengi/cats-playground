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

def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match {
  case List.Nil              => acc
  case List.Cons(head, tail) => f(head, foldRight(tail, acc, f))
}

// 3.9
def length[A](as: List[A]): Int = foldRight(as, 0, (_, acc) => acc + 1)

// 3.10
@tailrec
def foldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match {
  case List.Nil        => acc
  case List.Cons(h, t) => foldLeft(t, f(h, acc), f)
}

// 3.11
def product(as: List[Int]): Int = foldLeft(as, 1, _ * _)
def sum(as: List[Int]): Int = foldLeft(as, 0, _ + _)

// 3.12
def reverse[A](as: List[A]): List[A] = foldLeft(as, List.Nil, (a: A, b: List[A]) => List.Cons(a, b))

// 3.13
def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  foldLeft(as, (b: B) => b, (a: A, g: (b: B) => B) => b => g(f(a, b)))(acc)

// 3.14
def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2, (a, b) => List.Cons(a, b))

// 3.15
def flatten[A](as: List[List[A]]): List[A] = foldRight(as, List.Nil, (a: List[A], b: List[A]) => append(a, b))

// 3.16
def addOne(as: List[Int]): List[Int] = foldRight(as, List.Nil, (a: Int, b: List[Int]) => List.Cons(a + 1, b))

// 3.17
def doubleToString(as: List[Double]): List[String] = map(as, _.toString)

// 3.18
def map[A, B](as: List[A], f: (A => B)): List[B] = foldRight(as, List.Nil, (a: A, b: List[B]) => List.Cons(f(a), b))

// 3.19
def filter[A](as: List[A], f: (A => Boolean)): List[A] =
  foldRight(as, List.Nil, (a: A, b: List[A]) => if f(a) then List.Cons(a, b) else b)

// 3.20
def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
  foldRight(as, List.Nil, (a: A, b: List[B]) => append(f(a), b))

// 3.21
def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
  flatMap(as, a => if f(a) then List.Cons(a, List.Nil) else List.Nil)

// 3.22
def zipSum(l1: List[Int], l2: List[Int]): List[Int] = {
  (l1, l2) match {
    case (List.Nil, _)                          => List.Nil
    case (_, List.Nil)                          => List.Nil
    case (List.Cons(h1, t1), List.Cons(h2, t2)) => List.Cons(h1 + h2, zipSum(t1, t2))
  }
}

// 3.23
def zipOp[A](l1: List[A], l2: List[A], f: (A, A) => A): List[A] = {
  (l1, l2) match {
    case (List.Nil, l2)                         => l2
    case (l1, List.Nil)                         => l1
    case (List.Cons(h1, t1), List.Cons(h2, t2)) => List.Cons(f(h1, h2), zipOp(t1, t2, f))
  }
}

// 3.24
def hasSub[A](sup: List[A], sub: List[A]): Boolean = ???


object Test extends App {
  val l1: List[Int] = List(1, 2, 3, 4, 5)
  val l2: List[Int] = List(7, 8, 9)
  val l3 = List(l1, l2)

  println(l1)
  println(tail(l1))
  println(setHead(10, l1))
  println(drop(l1, 2))
  println(dropWhile(l1, _ < 3))
  println(init(l1))
  println(foldRight(l1, 0, _ + _))
  println(length(l1))
// fails with a  stack overflow error
//  println(length(List(1 to 20000*)))
  println(foldLeft(l1, 1, _ * _))
  println(product(l1))
  println(sum(l1))
  println(reverse(l1))
  println(append(l1, l2))
  println(flatten(l3))
  println(addOne(l1))
  println(doubleToString(List(1.0, 2.0)))
  println(filter(l1, _ % 2 == 0))
  println(filterViaFlatMap(l1, _ % 2 == 0))
  println(flatMap(l1, a => List.Cons(a, List.Cons(a + 10, List.Nil))))
  println(zipSum(l1, l2))
  println(zipOp(l1, l2, _ * _))
}
