import cats.data.State
import cats.syntax.all.*

object Calculator {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "*" => operator(_ * _)
      case "-" => operator(_ - _)
      case "/" => operator(_ / _)
      case _ => operand(sym.toInt)
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]){
      case (state, sym) => state.flatMap(_ => evalOne(sym))
    }

  def evalInput(input: String) = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  def operator(fn: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case b :: a :: tail =>
      val ans = fn(a, b)
      (ans :: tail, ans)
    case _ => sys.error("Fail!")
  }

  def operand(num: Int): CalcState[Int] = State[List[Int], Int] {
    stack => (num :: stack, 0)
  }

  def test = {
    val result = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      _ <- evalOne("+")
      _ <- evalOne("3")
      ans <- evalOne("*")
    } yield ans
    println(result.run(List()).value)

    val result2 = evalAll(List("1", "2", "+", "3", "*"))
    println(result2.runA(Nil).value)

    val result3 = evalInput("1 2 + 3 4 + *")
    println(result3)
  }
}
