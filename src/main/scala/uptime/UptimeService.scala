package uptime

import cats.Applicative
import cats.instances.list.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}
