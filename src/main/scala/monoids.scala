trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid

  given boolAndMonoid: Monoid[Boolean] with {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y

    override def empty: Boolean = true
  }

  given boolOrMonoid: Monoid[Boolean] with {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y

    override def empty: Boolean = false
  }

  given boolXorMonoid: Monoid[Boolean] with {
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)

    override def empty: Boolean = false
  }

  given boolNorMonoid: Monoid[Boolean] with {
    // T,T = T, F, F = T, * = F
    override def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)

    override def empty: Boolean = true
  }
  import scala.collection.immutable.Set
  given setUnionMonoid[A]: Monoid[Set[A]] with {
    def combine(x: Set[A], y: Set[A]) = x.union(y)
    def empty: Set[A] = Set.empty[A]
  }

  given setIntersactionMonoid[A]: Semigroup[Set[A]] with {
    def combine(x: Set[A], y: Set[A]) = x.intersect(y)
  }

  given symDiffMonoid[A]: Monoid[Set[A]] with {
    def combine(a: Set[A], b: Set[A]): Set[A] =
      a.diff(b).union(b.diff(a))

    def empty: Set[A] = Set.empty
  }

}
