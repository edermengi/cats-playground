package lightspeed

object FunctionalProgramming extends App {


  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val stringConcatenator = new Function[(String, String), String] {
    override def apply(v1: (String, String)): String = v1._1 + v1._2
  }

  val doubler: Int => Int = _ * 2

  // higher-order functions: take functions as arguments or/and return functions as results

  val allPairs = List(1, 2, 3).flatMap(number =>
    List('a', 'b', 'c').map(letter => s"$number-$letter")
  )
  println(allPairs)

  println(stringConcatenator("A", "B"))
}
