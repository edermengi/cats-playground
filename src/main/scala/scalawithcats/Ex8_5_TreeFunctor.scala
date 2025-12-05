package scalawithcats

import cats.*
import cats.syntax.functor.*

object Ex8_5_TreeFunctor extends App {
  sealed trait Tree[+A] // Covariant

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }
  implicit def treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](value: Tree[A])(func: A => B): Tree[B] =
        value match {
          case Leaf(a)             => Leaf(func(a))
          case Branch(left, right) => Branch(map(left)(func), map(right)(func))
        }
    }

  val t1 = Tree.branch(Tree.leaf(1), Tree.leaf(2))
  val t2 = t1.map(_ + 2)

  println(t1)
  println(t2)
}
