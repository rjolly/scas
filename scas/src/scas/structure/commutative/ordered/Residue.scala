package scas.structure.commutative.ordered

trait Residue[T : UniqueFactorizationDomain.Impl] extends Residue.Impl[T] with scas.structure.commutative.Residue[T] with UniqueFactorizationDomain[T]

object Residue {
  trait Impl[T](using ring: UniqueFactorizationDomain.Impl[T]) extends scas.structure.commutative.Residue.Impl[T] with UniqueFactorizationDomain.Impl[T] {
    def compare(x: T, y: T) = ring.compare(x, y)
    extension (x: T) override def signum = super.signum(x)
  }
}
