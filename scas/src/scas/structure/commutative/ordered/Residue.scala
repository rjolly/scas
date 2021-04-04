package scas.structure.commutative.ordered

trait Residue[T](using ring: UniqueFactorizationDomain[T]) extends scas.structure.commutative.Residue[T] with UniqueFactorizationDomain[T] {
  def compare(x: T, y: T) = ring.compare(x, y)
  extension (x: T) override def signum = super.signum(x)
}
