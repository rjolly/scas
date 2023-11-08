package scas.structure.commutative.ordered

trait Residue[T, R](using ring: UniqueFactorizationDomain[R]) extends scas.structure.commutative.Residue[T, R] with UniqueFactorizationDomain[T] {
  def compare(x: T, y: T) = {
    val this(a) = x: @unchecked
    val this(b) = y: @unchecked
    val this(c) = this(a): @unchecked
    val this(d) = this(b): @unchecked
    ring.compare(c, d)
  }
  extension (x: T) override def signum = super.signum(x)
}
