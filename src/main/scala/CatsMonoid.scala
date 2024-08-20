import cats.*
import cats.syntax.invariant.*
import cats.syntax.semigroup.*

object CatsMonoid {

  given symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)

  def test = {
    println(Monoid[Symbol].empty)
    println(Symbol("a") |+| Symbol("few") |+| Symbol("words"))
  }

}
