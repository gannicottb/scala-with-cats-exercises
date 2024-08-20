import cats.*
import cats.syntax.all.*

object functors {
  def test = {
    val list1 = List(1, 2, 3)
    // list1: List[Int] = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    // list2: List[Int] = List(2, 4, 6)

    val option1 = Option(123)
    // option1: Option[Int] = Some(value = 123)
    val option2 = Functor[Option].map(option1)(_.toString)
    // option2: Option[String] = Some(value = "123")

    val func = (x: Int) => x + 1
    // func: Function1[Int, Int] = repl.MdocSession$MdocApp0$$$Lambda/0x00007f555311ca10@34561cf6

    val liftedFunc = Functor[Option].lift(func)
    // liftedFunc: Function1[Option[Int], Option[Int]] = cats.Functor$$Lambda/0x00007f555311e000@6a9f032

    val liftedResult = liftedFunc(Option(1))
    // res1: Option[Int] = Some(value = 2)

    println(liftedResult)

    println(Tree.leaf(100).map(_ * 2))
    println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))
  }

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  given treeFunctor: Functor[Tree] with {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match
        case Branch(left, right) => Branch(left.map(f), right.map(f))
        case Leaf(value) => Leaf(f(value))
  }

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)

    given treeMonad: Monad[Tree] with {
      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
        fa match
          case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
          case Leaf(value) => f(value)
      }

      override def pure[A](x: A): Tree[A] = leaf(x)

      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
        flatMap(f(a)) {
          case Left(value) => tailRecM(value)(f)
          case Right(value) => leaf(value)
        }
    }

    def test = {
      import cats.syntax.all.*

      val flatMapped = branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
      println(flatMapped)

      val transformed = for {
        a <- branch(leaf(100), leaf(200))
        b <- branch(leaf(a - 10), leaf(a + 10))
        c <- branch(leaf(b - 1), leaf(b + 1))
      } yield c

      println(transformed)
    }
  }
}
