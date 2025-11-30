package scalawithcats

object Ex5_2_Expression extends App {

  enum Expression {
    case Add(left: Expression, right: Expression)
    case Subtract(left: Expression, right: Expression)
    case Multiply(left: Expression, right: Expression)
    case Divide(left: Expression, right: Expression)
    case Apply(num: Double)

    def +(that: Expression): Expression = Add(this, that)
    def -(that: Expression): Expression = Subtract(this, that)
    def *(that: Expression): Expression = Multiply(this, that)
    def /(that: Expression): Expression = Divide(this, that)

    def eval(): Double = this match {
      case Add(l, r)      => l.eval() + r.eval()
      case Subtract(l, r) => l.eval() - r.eval()
      case Multiply(l, r) => l.eval() * r.eval()
      case Divide(l, r)   => l.eval() / r.eval()
      case Apply(num)     => num
    }
  }

  object Expression {
    def apply(num: Double): Expression = Apply(num)
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
