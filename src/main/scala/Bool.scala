trait Bool {
  def `if`[A](t: A)(f: A): A
}
object Bool {
  val True: Bool = new Bool {
    def `if`[A](t: A)(f: A): A = t
  }

  val False: Bool = new Bool {
    def `if`[A](t: A)(f: A): A = f
  }

  def and(l: Bool, r: Bool): Bool =
    new Bool {
      def `if`[A](t: A)(f: A): A =
        l.`if`(r)(False).`if`(t)(f)
    }

  def or(l: Bool, r: Bool): Bool =
    new Bool {
      def `if`[A](t:  A)(f:  A): A =
        l.`if`(True)(r).`if`(t)(f)
    }

  def not(b: Bool): Bool =
    new Bool {
      def `if`[A](t: A)(f: A): A =
        b.`if`(False)(True).`if`(t)(f)
    }

  def truthTable2(fn: (Bool, Bool) => Bool) = {
    List(
      fn(True, True).`if`("yes")("no"),
      fn(True, False).`if`("yes")("no"),
      fn(False, True).`if`("yes")("no"),
      fn(False, False).`if`("yes")("no"),
    )
  }
  def truthTable1(fn: Bool => Bool) =
    List(
      fn(True).`if`("yes")("no"),
      fn(False).`if`("yes")("no"),
    )
  def test = {
    List(
      True.`if`("yes")("no"),
      False.`if`("yes")("no"),
      "-and-",
      truthTable2(and),
      "-or-",
      truthTable2(or),
      "-not-",
      truthTable1(not)
    ).foreach(println)
  }
}
