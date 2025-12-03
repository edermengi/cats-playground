package scalawithcats

import cats.*
import cats.syntax.all.*

object Ex6_3_CatEq extends App {

  final case class Cat(name: String, age: Int, color: String)

  given catEq: Eq[Cat] = Eq.instance { (x, y) => x.name == y.name && x.age == y.age && x.color == y.color }

  println(Cat("Bob", 1, "Black") === Cat("Bob", 1, "Black"))
  println(Cat("Bob", 1, "Black") === Cat("Bob", 1, "Black2"))
  println(Cat("Bob", 1, "Black") === Cat("Bob", 1, "Black2"))
}
