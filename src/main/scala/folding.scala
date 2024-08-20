import cats.Monoid
import cats.instances.list._

object folding {
  def map[A, B](l: List[A])(fn: A => B): List[B] =
    l.foldRight(List.empty[B])((i, accum) => fn(i) :: accum)

  def flatMap[A, B](l: List[A])(fn: A => List[B]): List[B] =
    l.foldRight(List.empty[B])((i, accum) => fn(i) ::: accum)

  def filter[A](l: List[A])(pred: A => Boolean): List[A] =
    l.foldRight(List.empty[A])((i, accum) => if(pred(i)) i :: accum else accum)

  def sumWithNumeric[A](l: List[A])(using numeric: Numeric[A]) =
    l.foldRight(numeric.zero)(numeric.plus)

  def sumWithMonoid[A](l: List[A])(using monoid: Monoid[A]) =
    l.foldRight(monoid.empty)(monoid.combine)

  def test = {
    val list = List(1,2,3)
    List(
      map(list)(_ + 1),
      flatMap(list)(i => List(i - 1, i, i + 1)),
      filter(list)(_ == 2),
      sumWithNumeric(list),
      sumWithMonoid(list)
    ).foreach(println)
  }

}
