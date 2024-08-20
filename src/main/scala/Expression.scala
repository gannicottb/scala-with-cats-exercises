
type Continuation = Double => Call
enum Call:
  case Continue(value: Double, k: Continuation)
  case Loop(expr: Expression, k: Continuation)
  case Done(result: Double)
enum Expression:
  case Literal(value: Double)
  case Addition(left: Expression, right: Expression)
  case Subtraction(left: Expression, right: Expression)
  case Multiplication(left: Expression, right: Expression)
  case Division(left: Expression, right: Expression)

  def eval: Double = this match {
    case Literal(value) => value
    case Addition(l, r) => l.eval + r.eval
    case Subtraction(l, r) => l.eval - r.eval
    case Multiplication(l, r) => l.eval * r.eval
    case Division(l, r) => l.eval / r.eval
  }

  def evalCPS: Double = {
    def loop(expr: Expression, cont: Continuation): Call = {
      expr match {
        case Literal(value) => Call.Continue(value, cont)
        case Addition(left, right) =>
          Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l + r, cont)))
        case Subtraction(left, right) =>
          Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l - r, cont)))
        case Multiplication(left, right) =>
          Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l * r, cont)))
        case Division(left, right) =>
          Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l / r, cont)))
      }
    }
    def trampoline(next: Call): Double =
      next match
        case Call.Continue(value, k) => trampoline(k(value))
        case Call.Loop(expr, k) => trampoline(loop(expr, k))
        case Call.Done(result) => result

    trampoline(loop(this, d => Call.Done(d)))
  }

  def `+`(r: Expression) = Addition(this, r)
  def `-`(r: Expression) = Subtraction(this, r)
  def `*`(r: Expression) = Multiplication(this, r)
  def `/`(r: Expression) = Division(this, r)


object Expression {
  def apply(value: Double): Expression = Literal(value)

  def test = {
    val expr = (Literal(1) + Literal(2)) - (Literal(2) * Literal(3))
    val fortyTwo = ((Expression(15.0) + Expression(5.0)) * Expression(2.0) + Expression(2.0)) / Expression(1.0)
    println(s"(1 + 2) - (2 * 3) = ${expr.evalCPS}")
    println(s"${fortyTwo.evalCPS}")
  }
}
