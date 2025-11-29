package scalawithcats

object Ex3_4_Codata_Or extends App {

  trait Bool {
    def `if`[A](t: A)(f: A): A
  }

  val True = new Bool {
    def `if`[A](t: A)(f: A): A = t
  }
  val False = new Bool {
    def `if`[A](t: A)(f: A): A = f
  }

  private def and(l: Bool, r: Bool) = new Bool {
    def `if`[A](t: A)(f: A): A = l.`if`(r.`if`(t)(f))(f)
  }

  private def or(l: Bool, r: Bool) = new Bool {
    def `if`[A](t: A)(f: A): A = l.`if`(t)(r.`if`(t)(f))
  }

  private def not(l: Bool) = new Bool {
    def `if`[A](t: A)(f: A): A = l.`if`(f)(t)
  }

  println("AND:")
  println(and(True, True).`if`("yes")("no"))
  println(and(True, False).`if`("yes")("no"))
  println(and(False, True).`if`("yes")("no"))
  println(and(False, False).`if`("yes")("no"))

  println("OR:")
  println(or(True, True).`if`("yes")("no"))
  println(or(True, False).`if`("yes")("no"))
  println(or(False, True).`if`("yes")("no"))
  println(or(False, False).`if`("yes")("no"))

  println("NOT:")
  println(not(True).`if`("yes")("no"))
  println(not(False).`if`("yes")("no"))
}
