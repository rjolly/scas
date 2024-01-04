package scas.structure.commutative.ordered

trait Residue[T, R](using ring: UniqueFactorizationDomain[R]) extends scas.structure.commutative.Residue[T, R] with UniqueFactorizationDomain[T] {
  def compare(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    val self(c) = this(a)
    val self(d) = this(b)
    ring.compare(c, d)
  }
  extension (x: T) override def signum = super.signum(x)
}
