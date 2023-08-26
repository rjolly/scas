package scas.structure.commutative.ordered.impl

trait Residue[T](using ring: UniqueFactorizationDomain[T]) extends scas.structure.commutative.impl.Residue[T] with UniqueFactorizationDomain[T] {
  def compare(x: T, y: T) = ring.compare(x, y)
  extension (x: T) override def signum = super.signum(x)
}
