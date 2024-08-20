trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }
}

object Codec {
//  given Codec[Double] with
//    override def encode(value: Double): String = value.toString
//
//    override def decode(value: String): Double = value.toDouble

  given intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  given booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  given Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

  given stringCodec: Codec[String] with {
    def encode(value: String): String = value

    def decode(value: String): String = value
  }

  given boxCodec[A](using c: Codec[A]): Codec[Box[A]] = c.imap(Box.apply, _.value)



  def test = {
    import CodecSyntax.*
    println(Box(true).encode)
    println(Box(123.4).encode)
    val decodeBox = summon[Codec[Box[Double]]]
    println(decodeBox.decode("123.4"))
  }
}

object CodecSyntax {
  extension [A](a: A)(using c: Codec[A]) {
    def encode: String = c.encode(a)
  }

}
