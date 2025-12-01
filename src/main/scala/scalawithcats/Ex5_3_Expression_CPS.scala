package scalawithcats

object Ex5_3_Expression_CPS extends App {

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
      type Continuation = Double => Double

      def eval0(expr: Expression, cont: Continuation): Double = expr match {
        case Add(left, right)      => eval0(left, l => eval0(right, r => cont(l + r)))
        case Subtract(left, right) => eval0(left, l => eval0(right, r => cont(l - r)))
        case Multiply(left, right) => eval0(left, l => eval0(right, r => cont(l * r)))
        case Divide(left, right)   => eval0(left, l => eval0(right, r => cont(l / r)))
        case Literal(num)          => cont(num)
      }

      eval0(this, identity)
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

  println(ex1.eval())
  println(ex2.eval())
  println(ex3.eval())
  println(ex4.eval())
  println(ex5.eval())
}
