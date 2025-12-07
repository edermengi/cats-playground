package scalawithcats

object Ex8_6_Imap extends App {

  trait Codec[A] {
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      def encode(value: B): String = Codec.this.encode(enc(value))
      def decode(value: String): B = dec(Codec.this.decode(value))
    }
  }

  def encode[A](value: A)(using c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(using c: Codec[A]): A = c.decode(value)

  given stringCodec: Codec[String] with {
    def encode(value: String): String = s"`$value`"
    def decode(value: String): String = value
  }

  final case class Box[A](value: A)

  given intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
  given booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
  given doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
  given boxCodec[A](using c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_), _.value)

  println(encode(3.5))
  println(decode[Double]("3.5"))

  println(encode(Box(123.4)))
  println(encode(Box(true)))
  println(decode[Box[Double]]("300.0"))
}
