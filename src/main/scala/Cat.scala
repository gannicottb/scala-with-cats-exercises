import cats.{Eq, Show}
import cats.syntax.all.*

final case class Cat(name: String, age: Int, color: String)
object Cat {
  import DisplaySyntax.*
  given Display[Cat] with
    def display(cat: Cat) = {
      val name = cat.name.display
      val age = cat.age.display
      val color = cat.color.display
      s"$name is a $age year-old $color cat."
    }

  given catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  given catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    def checkField[A: Eq](cat1: Cat, cat2: Cat)(accessor: Cat => A): Boolean =
      accessor(cat1) === accessor(cat2)

    Seq(
      checkField(cat1, cat2)(_.name),
      checkField(cat1, cat2)(_.age),
      checkField(cat1, cat2)(_.color),
    ).reduce(_ && _)

  }

  def test = {
    println(Cat("Garfield", 38, "ginger and black").show)
    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    def printEqTest[A: Eq](l: A, r: A) = {
      println(s"$l === $r? ${l === r}")
      println(s"$l =!= $r? ${l =!= r}")
    }

    printEqTest(cat1, cat2)
    printEqTest(optionCat1, optionCat2)


  }
}
