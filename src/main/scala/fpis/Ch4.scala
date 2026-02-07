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
}
