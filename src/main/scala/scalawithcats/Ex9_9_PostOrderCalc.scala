package scalawithcats

import cats.data.State
import cats.syntax.applicative.*

object Ex9_9_PostOrderCalc extends App {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (state, sym) =>
      state.flatMap(_ => evalOne(sym))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(evalInput("2 3 +"))
  println(evalInput("1"))
  println(evalInput("1 3 7"))
}
