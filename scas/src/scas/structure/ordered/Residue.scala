package scas.structure.ordered

import scala.compiletime.deferred

trait Residue[T, R] extends scas.structure.Residue[T, R] with Ring[T] {
  given ring: Ring[R] = deferred
  def compare(x: T, y: T) = {
    val self(a) = x.runtimeChecked
    val self(b) = y.runtimeChecked
    val self(c) = this(a).runtimeChecked
    val self(d) = this(b).runtimeChecked
    ring.compare(c, d)
  }
  extension (x: T) override def signum = super.signum(x)
}
