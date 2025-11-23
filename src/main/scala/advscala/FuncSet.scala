package advscala

import scala.annotation.tailrec
import scala.runtime.Nothing$

object FuncSet extends App {

  abstract class FSet[A] extends (A => Boolean) {
    def contains(el: A): Boolean

    def apply(el: A): Boolean = contains(el)

    infix def +(e: A): FSet[A]

    infix def ++(other: FSet[A]): FSet[A]

    def map[B](f: A => B): FSet[B]

    def flatMap[B](f: A => FSet[B]): FSet[B]

    def filter(predicate: A => Boolean): FSet[A]

    def foreach(f: A => Unit): Unit
  }

  case class Empty[A]() extends FSet[A] {

    override def contains(el: A): Boolean = false

    override infix def +(e: A): FSet[A] = Cons(e, this)

    override infix def ++(other: FSet[A]): FSet[A] = other

    override def map[B](f: A => B): FSet[B] = Empty()

    override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()

    override def filter(predicate: A => Boolean): FSet[A] = this

    override def foreach(f: A => Unit): Unit = ()
  }

  case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {

    override def contains(el: A): Boolean = head == el || tail.contains(el)

    override infix def +(e: A): FSet[A] =
      if (contains(e)) this
      else Cons(e, this)

    override infix def ++(other: FSet[A]): FSet[A] = other match {
      case Empty()          => this
      case Cons(head, tail) => tail ++ (this + head)
    }

    override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)

    override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)

    override def filter(predicate: A => Boolean): FSet[A] = {
      val filteredTail = tail.filter(predicate)
      if (predicate(head)) filteredTail + head
      else filteredTail
    }

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }
  }

  object FSet {
    def apply[A](values: A*): FSet[A] = {
      @tailrec
      def buildSet(valuesSeq: Seq[A], acc: FSet[A]): FSet[A] =
        if (valuesSeq.isEmpty) acc
        else buildSet(valuesSeq.tail, acc + valuesSeq.head)

      buildSet(values, Empty())
    }

  }

  val nums = FSet(1, 2, 3)
  val strs = nums.map(_ + "0")

  val s1 = Cons(1, Empty())
  val s2 = Cons(2, Empty())
  val s3 = s1 ++ s2
}
