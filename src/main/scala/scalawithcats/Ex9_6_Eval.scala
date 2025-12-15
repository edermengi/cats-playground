package scalawithcats

import cats.Eval

import scala.util.{Failure, Success, Try}

object Ex9_6_Eval extends App {

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        foldRight(tail, acc)(fn).map(b => fn(head, b))
      case Nil =>
        Eval.now(acc)
    }

  println(foldRight(List(1, 2, 3, 4), 0)(_ + _).value)

}
