package scas.structure.commutative.ordered

trait Residue[T, R](using ring: UniqueFactorizationDomain[R]) extends scas.structure.commutative.Residue[T, R] with UniqueFactorizationDomain[T] {
  def compare(x: T, y: T) = {
    val self(a) = x: @unchecked
    val self(b) = y: @unchecked
    val self(c) = this(a): @unchecked
    val self(d) = this(b): @unchecked
    ring.compare(c, d)
  }
  extension (x: T) override def signum = super.signum(x)
}
