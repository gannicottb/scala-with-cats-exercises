import cats.Monoid
import cats.syntax.all.*
import cats.instances.*

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object mapreduce {
  def foldMap[A, B: Monoid](as: Vector[A])(fn: A => B): B =
    as.foldLeft(Monoid.empty[B])(_ |+| fn(_))


  def foldMapPar[A, B: Monoid](as: Vector[A])(fn: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val batchSize = (as.size.toDouble / numCores).ceil.toInt
    val futures =
      as.grouped(batchSize)
        .map(batch => Future{foldMap(batch)(fn)})

    Future.sequence(futures) map { iterable =>
      iterable.foldLeft(Monoid.empty[B])(_ |+| _)
    }
  }

  def foldMapParCats[A, B: Monoid](as: Vector[A])(fn: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val batchSize = (as.size.toDouble / numCores).ceil.toInt

    as
      .grouped(batchSize) // make Iter[Vec[A]]
      .toVector // Vec[Vec[A]]
      .traverse(batch => Future(batch.foldMap(fn))) // Fut[Vec[B]]
      .map(_.combineAll) // Fut[B] (Vec[B] => B, because B is a monoid and we can combine freely)
  }


  def test = {
    List(
    foldMap(Vector(1, 2, 3))(identity),
    foldMap(Vector(1, 2, 3))(_.toString + "! "),
    foldMap("Hello world!".toVector)(_.toString.toUpperCase)
    ).foreach(println)

    List(
      foldMapPar(Vector(1, 2, 3))(identity),
      foldMapPar(Vector(1, 2, 3))(_.toString + "! "),
      foldMapPar("Hello world!".toVector)(_.toString.toUpperCase)
    ).foreach { z =>
      println(Await.result(z, 1.second))
    }

    val result: Future[Int] =
      foldMapPar((1 to 1000000).toVector)(identity)
    println(Await.result(result, 1.second))

    val result2: Future[Int] =
      foldMapParCats((1 to 1000).toVector)(_ * 1000)
    println(Await.result(result2, 1.second))
  }
}
