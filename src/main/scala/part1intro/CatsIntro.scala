package part1intro

object CatsIntro {

  // Eq
  //  val aComparison = 2 == "a string"

  // part 1 - type class import

  import cats.Eq

  // part 2 - import TC instances for the types you need
  //  import cats.instances.int._

  // part 3 - use the TC API

  val intEquality = Eq[Int]
  val stringEquality = Eq[String]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
  val aTypeSafeComparison3 = stringEquality.eqv("2", "3") // false
  // val aUnsafeComparison = intEquality.eqv(2, "3") // does not compile

  // part 4 - use extension methods (if applicable)

  import cats.syntax.eq.*

  val anotherTypeSafeSomparison = 2 === 3
  val neqSomparison = 2 =!= 3
  //  val invalidComparison = 2 === "a String" -- does not compile

  // part5 - extending the TC operations to composite types

  //  import cats.instances._

  //  import cats.instances.list._

  val aListComparison = List(2) === List(3)

  // part 6 - create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 23.22) === ToyCar("Lamborghini", 23.22)

  def main(args: Array[String]): Unit = {
    println(aListComparison)
  }
}
