package uptime

import cats.Id

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}
