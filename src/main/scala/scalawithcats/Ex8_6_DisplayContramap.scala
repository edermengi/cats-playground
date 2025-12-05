package scalawithcats

object Ex8_6_DisplayContramap extends App {

  trait Display[A] {
    def display(value: A): String

    def contramap[B](func: B => A): Display[B] =
      (value: B) => Display.this.display(func(value))
  }

  given stringDisplay: Display[String] with {
    def display(value: String): String =
      s"'${value}'"
  }

  given booleanDisplay: Display[Boolean] with {
    def display(value: Boolean): String =
      if value then "yes" else "no"
  }

  def display[A](value: A)(using p: Display[A]): String =
    p.display(value)

  println(display("hello"))
  println(display(true))

  final case class Box[A](value: A)

  given boxDisplay[A](using d: Display[A]): Display[Box[A]] =
    d.contramap[Box[A]](_.value)

  println(display(Box("hello world")))
  println(display(Box(false)))
}
