import cats.*
import cats.syntax.all.*
import scala.util.Try

object abstracting {

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if(age >= 18) age.pure[F]
    else new IllegalArgumentException("Not an adult!").raiseError[F, Int]

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)){
      case (a, memo) => memo.map(fn(a, _))
    }.value

  def foldRightEval[A,B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def test = {
    println(validateAdult[Try](18))
    println(validateAdult[Try](8))
    type ExceptionOr[A] = Either[Throwable, A]
    println(validateAdult[ExceptionOr](-1))
    println(foldRight((1 to 100000).toList, 0L)(_ + _))
  }
}
