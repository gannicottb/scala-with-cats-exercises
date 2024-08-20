trait Stream[A] {
  def head: A
  def tail: Stream[A]

  def map[B](f: A => B): Stream[B] = {
    val self = this
    new Stream[B] {
      def head: B = f(self.head)

      def tail: Stream[B] = self.tail.map(f)
    }
  }

  def take(count: Int): List[A] =
    count match {
      case 0 => Nil
      case n => head :: tail.take(n - 1)
    }

  def filter(pred: A => Boolean): Stream[A] = {
    val self = this
    new Stream[A]{
      def head = {
        def loop(s: Stream[A]): A =
          if(pred(s.head)) s.head else loop(s.tail)
        loop(self)
      }
      def tail = self.tail.filter(pred)
    }
  }

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    val self = this
    new Stream[(A, B)]{
      def head = self.head -> that.head
      def tail = self.tail.zip(that.tail)
    }
  def scanLeft[B](zero: B)(f: (B, A) => B): Stream[B] =
    val self = this
    new Stream[B]{
      def head = f(zero, self.head)
      def tail = self.tail.scanLeft(this.head)(f)
    }
}
object Stream {
  def unfold[A, B](seed: A, f: A => B, next: A => A): Stream[B] =
    new Stream[B]{
      def head: B =
        f(seed)
      def tail: Stream[B] =
        unfold(next(seed), f, next)
    }

  def ones: Stream[Int] = new Stream[Int] {
    def head: Int = 1

    def tail: Stream[Int] = ones
  }

  def always[A](elt: => A): Stream[A] =
    new Stream[A] {
      lazy val head: A = elt
      lazy val tail: Stream[A] = always(head)
    }
    
  def print5[A](stream: Stream[A]) = println(s"${stream.take(5)}")
  def test: Unit = {
    val alternating = Stream.unfold(
      true,
      x => if x then 1 else -1,
      x => !x
    )
    val increasing = Stream.unfold(
      0,
      identity,
      _ + 1
    )

    List(
      alternating,
      alternating.filter(_ > 0),
      increasing,
      increasing.zip(alternating),
      increasing.scanLeft(0)(_ + _),
      Stream.ones.scanLeft(0)(_ + _),
      Stream.always(2)
    ).foreach(print5)
  }
}
