import cats.*
import cats.syntax.all.*

case class Order(totalCost: Double, quantity: Double)

object SuperAdder {

  given orderMonoid: Monoid[Order] with {
    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    override def empty = Order(0.0, 0)
  }


  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)


  def test: Unit = {
    println(add(List(Some(1), None, Some(2))))
    println(add(List(
      Order(100.0, 2.0),
      Order(10.0, 5.0)
    )))
  }

}
