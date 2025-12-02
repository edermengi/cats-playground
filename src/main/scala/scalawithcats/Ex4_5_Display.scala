package scalawithcats

object Ex4_5_Display extends App {

  trait Display[A] {
    def display(value: A): String
  }

  object Display {
    given Display[String] with
      def display(value: String): String = value

    given Display[Int] with
      def display(value: Int): String = value.toString

    given Display[Cat] with
      def display(value: Cat): String = s"NAME is ${value.name} AGE ${value.age} COLOR ${value.color}."

    def print[A](value: A)(using display: Display[A]): Unit = {
      println(display.display(value))
    }
  }

  object DisplaySyntax {
    extension [A](value: A)(using d: Display[A]) {
      def display(): String = d.display(value)
      def print(): Unit = Display.print(value)
    }
  }

  final case class Cat(name: String, age: Int, color: String)

  Display.print("Hi there")
  Display.print(2)
  Display.print(Cat("Bob", 3, "Gray"))

  import DisplaySyntax.*

  Cat("Dymka", 5, "Black").print()
}
