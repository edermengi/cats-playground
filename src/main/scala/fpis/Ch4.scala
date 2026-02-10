package fpis

import fpis.Ch4.Opt.{No, So}
import scala.List
import scala.Nil

object Ch4 extends App {

  enum Opt[+A] {
    case No
    case So(value: A)

    def map[B](f: A => B): Opt[B] = this match {
      case No        => No
      case So(value) => So(f(value))
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case No        => default
      case So(value) => value
    }
    def orElse[B >: A](default: => Opt[B]): Opt[B] = map(So(_)).getOrElse(default)
    def filter(f: A => Boolean): Opt[A] = if map(f).getOrElse(false) then this else No
    def flatMap[B](f: A => Opt[B]): Opt[B] = map(f).getOrElse(No)
  }

  // 4.2
  def mean(xs: Seq[Double]): Opt[Double] =
    if xs.isEmpty then No
    else So(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Opt[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aV <- a
      bV <- b
    } yield f(aV, bV)

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil))((a: Option[A], b: Option[List[A]]) => map2(a, b)(_ :: _))

  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil))((a: A, b: Option[List[B]]) => map2(f(a), b)(_ :: _))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)

  println(So(0).map(_ + 1))
  println(No.getOrElse(2))
  println(So(3).getOrElse(30))
  println(So(4).filter(_ == 4))
  println(So(5).flatMap(a => So(a + "+")))
  println(variance(Seq(1, 2, 3, 4, 5, 10)))

  println(map2(Some(1), Some(2))(_ + _))
  println(map2(Some(1), Option.empty[Int])(_ + _))
  println("traverse: " + traverse(List("1", "2", "3"))(_.toIntOption))
  println("traverse: " + traverse(List("1", "2", "a"))(_.toIntOption))
  println("sequence: " + sequence(List(Some(1), Some(2))))
  println("sequence: " + sequence(List(Some(1), None)))
  println("sequence2: " + sequence2(List(Some(1), Some(2))))
  println("sequence2: " + sequence2(List(Some(1), None)))

  enum Either[+E, +A]:
    case Left(value: E)
    case Right(value: A)

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e)  => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case _       => this
    }

    def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, that) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(ea), _)        => Left(ea)
      case (_, Left(eb))        => Left(eb)
    }

  import Either.*

  println(Left("error").orElse(Right("Not error")))
  println(Right(1).map(_ + 10))
  println(Right(1).map2(Right(3))(_ + _))

}
