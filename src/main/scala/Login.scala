import cats.data.Reader
import cats.syntax.all.*

object Login {
  final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]


  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(
    username: String,
    password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))


  def checkLogin(
    userId: Int,
    password: String): DbReader[Boolean] =
    for {
      userName <- findUsername(userId)
      check <- userName.map(un =>
        checkPassword(un, password)
      ).getOrElse(false.pure[DbReader])
    } yield check

  def test = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    List(
      checkLogin(1, "zerocool").run(db),
      checkLogin(4, "davinci").run(db)
    ).foreach(println)
  }
}
