import functors.Tree
import uptime.UptimeServiceSpec

@main
def main(): Unit = {
  Stream.test
  Bool.test
  Set.test
  Display.test
  Cat.test
  Expression.test
  SuperAdder.test
  functors.test
  Codec.test
  CatsMonoid.test
  abstracting.test
//  writer.test
  Login.test
  Calculator.test
  Tree.test
  Autobots.test
  parallel.test
  folding.test
  traverse.test
  UptimeServiceSpec.testTotalUptime()
  mapreduce.test
}
