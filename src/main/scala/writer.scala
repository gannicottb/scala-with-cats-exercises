import cats.data.{Writer, WriterT}
import cats.syntax.all.*
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.*
import scala.concurrent.duration.*

object writer {
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialW(n: Int): Logged[Int] = {
    (if (n == 0) 1.pure[Logged] else slowly(factorialW(n - 1).map(_ * n)))
      .flatMap { ans =>
        Vector(s"fact $n $ans").tell.map(_ => ans)
      }
  }

  def factorialW_(n: Int): Logged[Int] = {
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorialW_(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def test = {
    val res = Await.result(Future.sequence(Vector(
      Future(factorial(5)),
      Future(factorial(5))
    )), 5.seconds)

    println(s"Result: $res")

    val res2 = Await.result(Future.sequence(Vector(
      Future(factorialW(5)),
      Future(factorialW(5))
    )), 5.seconds)

    println(s"Writer Result: $res2")

    val res3 = Await.result(Future.sequence(Vector(
      Future(factorialW_(5)),
      Future(factorialW_(5))
    )).map(_.map(_.written)), 5.seconds)

    println(s"Writer_ Result: $res3")
  }
}
