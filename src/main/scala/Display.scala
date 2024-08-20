import cats.*
import cats.syntax.all.*

trait Display[A] { self =>
  def display(a: A): String

  def contramap[B](func: B => A): Display[B] = {
    new Display[B] {
      def display(value: B): String =
        self.display(func(value))
    }
  }
}
object Display {
  given Display[String] with
    def display(a: String) = a

  given Display[Int] with
    def display(i: Int) = i.toString

  given Display[Boolean] with {
    def display(value: Boolean): String =
      if value then "yes" else "no"
  }

  def display[A](a: A)(using d: Display[A]) = d.display(a)

  def print[A](a: A)(using d: Display[A]) = println(display(a))

  given displayBox[A](using d: Display[A]): Display[Box[A]] = d.contramap(_.value)

  def test: Unit = {
    print("string")
    print(100)
    print(Cat("Crom", 8, "Orange"))
    print(Box("hello world"))
    print(Box(true))
    print(Box(123))
  }
}

final case class Box[A](value: A)

object DisplaySyntax {
  extension [A](a: A)(using d: Display[A]) {
    def display: String = d.display(a)
    def print: Unit = Display.print(a)
  }
}
