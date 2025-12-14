package scalawithcats

import cats.MonadError

import scala.util.{Success, Try, Failure}

object Ex9_5_MonadError extends App {

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    if (age >= 18) me.pure(age)
    else me.raiseError(new IllegalArgumentException("Age must be greater or equal then 18"))
  }

  assert(validateAdult[Try](18) == Success(18))
  assert(validateAdult[Try](16) match {
    case Failure(exception) => true
    case _                  => false
  })

  type ExceptionOr[A] = Either[Throwable, A]

  println(validateAdult[ExceptionOr](-1))

}
