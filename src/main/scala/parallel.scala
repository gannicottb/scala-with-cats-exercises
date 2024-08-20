import cats.instances.list.*
import cats.syntax.all.*

object parallel {
  val x = (List(1,2), List(3,4))
  
  def test = {
    println(x.tupled)
    println(x.parTupled)  
  }
  
}
