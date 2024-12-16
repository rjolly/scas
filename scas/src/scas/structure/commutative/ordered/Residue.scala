package scas.structure.commutative.ordered

import scala.compiletime.deferred

trait Residue[T, R] extends scas.structure.commutative.Residue[T, R] with UniqueFactorizationDomain[T] {
  given ring: () => UniqueFactorizationDomain[R] = deferred
  def compare(x: T, y: T) = {
    val a = x.unapply
    val b = y.unapply
    val c = this(a).unapply
    val d = this(b).unapply
    ring.compare(c, d)
  }
  extension (x: T) override def signum = super.signum(x)
}
