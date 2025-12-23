package scalawithcats

import cats.data.Writer
import cats.implicits.catsSyntaxApplicativeId
import cats.instances.vector.*
import cats.syntax.writer.*

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Ex9_7_Writer extends App {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(10)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
               1.pure[Logged]
             } else {
               factorial(n - 1).map(_ * n)
             }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val result = factorial(5).run
  println(result)

  val result2 = Await.result(
    Future
      .sequence(
        Vector(
          Future(factorial(5)),
          Future(factorial(5))
        )
      )
      .map(_.map(_.written)),
    5.seconds
  )
  println(result2)

}
