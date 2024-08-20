trait Set[A] {

  /** True if this set contains the given element */
  def contains(elt: A): Boolean

  /** Construct a new set containing the given element */
  def insert(elt: A): Set[A] = InsertOneSet(elt, this)

  /** Construct the union of this and that set */
  def union(that: Set[A]): Set[A] = UnionSet(this, that)
}

final class InsertOneSet[A](element: A, source: Set[A])
  extends Set[A] {

  def contains(elt: A): Boolean =
    elt == element || source.contains(elt)
}

final class UnionSet[A](first: Set[A], second: Set[A])
  extends Set[A] {

  def contains(elt: A): Boolean =
    first.contains(elt) || second.contains(elt)
}

object Evens extends Set[Int] {
  def contains(elt: Int) = elt % 2 == 0
}

final class ListSet[A](elements: List[A]) extends Set[A] {

  def contains(elt: A): Boolean =
    elements.contains(elt)

  override def insert(elt: A): Set[A] =
    ListSet(elt :: elements)

  override def union(that: Set[A]): Set[A] =
    elements.foldLeft(that) { (set, elt) => set.insert(elt) }
}
object ListSet {
  def empty[A]: Set[A] = ListSet(List.empty)
}

final class IndicatorSet[A](indicator: A => Boolean)
  extends Set[A] {

  def contains(elt: A): Boolean =
    indicator(elt)
}

object Set {
  def test = {
    val evensAndOne = Evens.insert(1)
    val evensAndOthers =
      Evens.union(ListSet.empty.insert(1).insert(3))
    val odds = IndicatorSet[Int](_ % 2 == 1)
    Seq(
      evensAndOne -> "evensAndOne",
      evensAndOthers -> "evensAndOthers",
      odds -> "odds",
      Evens.union(odds) -> "integers"
    ).flatMap{
      case (set, label) =>
        Seq[(Set[Int] => Boolean, String)](
          (_.contains(1),"contains(1)"),
          (_.contains(2),"contains(2)"),
          (_.contains(3),"contains(3)")
        ).map{
          case (fn, note) =>
            s"$label.$note: ${fn(set)}"
        }
    }.foreach(println)




  }
}
