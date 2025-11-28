package scalawithcats

object Ex3_3_StreamCombinators extends App {
  trait Stream[A] {
    def head: A
    def tail: Stream[A]

    def take(count: Int): List[A] = count match {
      case 0 => Nil
      case n => head :: tail.take(n - 1)
    }

    def filter(pred: A => Boolean): Stream[A] = {
      if (pred(head)) {
        new Stream {
          def head: A = Stream.this.head
          def tail: Stream[A] = Stream.this.tail.filter(pred)
        }
      } else
        tail.filter(pred)
    }
    def zip[B](that: Stream[B]): Stream[(A, B)] = new Stream {
      def head: (A, B) = (Stream.this.head, that.head)
      def tail: Stream[(A, B)] = Stream.this.tail.zip(that.tail)
    }
    def scanLeft[B](zero: B)(f: (B, A) => B): Stream[B] = {
      val self = this
      new Stream {
        def head: B = zero
        def tail: Stream[B] = {
          val nextAcc = f(zero, self.head)
          self.tail.scanLeft(nextAcc)(f)
        }
      }
    }
  }

  private object Stream {
    def unfold[A, B](seed: A, f: A => B, next: A => A): Stream[B] =
      new Stream[B] {
        def head: B = f(seed)
        def tail: Stream[B] = unfold(next(seed), f, next)
      }
  }

  private val numbers = Stream.unfold(1, identity, _ + 1)
  private val letters = Stream.unfold('a', identity, ch => (ch + 1).toChar)

  println(numbers.take(10))
  println(numbers.filter(_ % 2 == 0).take(5))
  println(letters.take(10))
  println(letters.zip(numbers).take(10))

  println(letters.scanLeft("")((acc, el) => acc + el).take(10))
}
