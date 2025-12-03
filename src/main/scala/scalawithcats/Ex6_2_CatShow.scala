package scalawithcats

import cats.*
import cats.syntax.all.*

object Ex6_2_CatShow extends App {

  final case class Cat(name: String, age: Int, color: String)

  given catShow: Show[Cat] = Show.show(cat => s"NAME is ${cat.name} AGE ${cat.age} COLOR ${cat.color}.")

  println(Cat("Bob", 1, "Black").show)
}
