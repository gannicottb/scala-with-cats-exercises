import cats.data.EitherT
import cats.syntax.all.*
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Autobots {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot).map(lvl =>
      EitherT.right(Future.successful(lvl))
    ).getOrElse(
      EitherT.left(Future.successful(s"Could not determine power level of $autobot"))
    )

  def canSpecialMove(a: String, b: String): Response[Boolean] =
    for {
      aLevel <- getPowerLevel(a)
      bLevel <- getPowerLevel(b)
    } yield (aLevel + bLevel) > 15

  def tacticalReport(a: String, b: String): String = {
    val x = canSpecialMove(a, b).value.map {
      case Left(msg) => msg
      case Right(true) => s"$a and $b can do a special move"
      case Right(false) => s"$a and $b need a recharge"
    }
    Await.result(x, 1.second)
  }

  def test = {
    List(
      tacticalReport("Jazz", "Bumblebee"),
      tacticalReport("Bumblebee", "Hot Rod"),
      tacticalReport("Jazz", "Ironhide")
    ).foreach(println)
  }
}
