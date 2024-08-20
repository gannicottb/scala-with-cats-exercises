import cats.Applicative
import cats.instances.vector.* // for Applicative
import cats.instances.option.* // for Applicative
import cats.data.Validated
import cats.instances.list._ // for Monoid
import cats.syntax.all.*



object traverse {

  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]

  def processV(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }


  def test = {
    val s: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4)))
    println(s)
    println(
      listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
    )
    List(
      process(List(2, 4, 6)), // Some(List(2,4,6))
      process(List(1, 2, 3)) // None
    ).foreach(println)

    List(
      processV(List(2, 4, 6)), // Valid(List(2,4,6))
      processV(List(1, 2, 3)) // Invalid(List("1 is not even", "3 is not even"))
    ).foreach(println)
  }
}
