package scalawithcats

import scala.annotation.tailrec

object Ex5_3_Expression_Trampoline extends App {

  enum Expression {
    case Add(left: Expression, right: Expression)
    case Subtract(left: Expression, right: Expression)
    case Multiply(left: Expression, right: Expression)
    case Divide(left: Expression, right: Expression)
    case Literal(num: Double)

    def +(that: Expression): Expression = Add(this, that)
    def -(that: Expression): Expression = Subtract(this, that)
    def *(that: Expression): Expression = Multiply(this, that)
    def /(that: Expression): Expression = Divide(this, that)

    def eval(): Double = {
      type Continuation = Double => Call

      enum Call {
        case Loop(expression: Expression, continuation: Continuation)
        case Continue(value: Double, continuation: Continuation)
        case Done(value: Double)
      }

      def eval0(expr: Expression, cont: Continuation): Call = expr match {
        case Add(left, right)      => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l + r, cont)))
        case Subtract(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l - r, cont)))
        case Multiply(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l * r, cont)))
        case Divide(left, right)   => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l / r, cont)))
        case Literal(num)          => Call.Continue(num, cont)
      }

      @tailrec
      def trampoline(next: Call): Double =
        next match {
          case Call.Loop(expression, continuation) =>
            trampoline(eval0(expression, continuation))
          case Call.Continue(value, continuation) =>
            trampoline(continuation(value))
          case Call.Done(value) => value
        }

      trampoline(eval0(this, v => Call.Done(v)))
    }

  }

  object Expression {
    def apply(num: Double): Expression = Literal(num)
  }

  val ex1 = Expression(2.0)
  val ex2 = Expression(2.0) + Expression(3.0)
  val ex3 = Expression(2.0) * Expression(3.0)
  val ex4 = Expression(3.0) / Expression(6.0)
  val ex5 = Expression(3.0) + Expression(6.0) / Expression(3.0) + Expression(1.0)
  val ex6 = Expression(3.0) * Expression(6.0) / Expression(3.0) + Expression(1.0) * Expression(5.0)

  println(ex1.eval())
  println(ex2.eval())
  println(ex3.eval())
  println(ex4.eval())
  println(ex5.eval())
  println(ex6)
  println(ex6.eval())
}
