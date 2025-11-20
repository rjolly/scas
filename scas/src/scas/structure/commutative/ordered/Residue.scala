package scas.structure.commutative.ordered

import scala.compiletime.deferred

trait Residue[T, R] extends scas.structure.commutative.Residue[T, R] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[R] = deferred
  def compare(x: T, y: T) = {
    val self(a) = x.runtimeChecked
    val self(b) = y.runtimeChecked
    val self(c) = this(a).runtimeChecked
    val self(d) = this(b).runtimeChecked
    ring.compare(c, d)
  }
  extension (x: T) override def signum = super.signum(x)
}
