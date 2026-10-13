package scas.structure.commutative

import scala.compiletime.deferred

trait Residue[T, R] extends scas.structure.Residue[T, R] with UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[R] = deferred
  extension (x: T) {
    override def divide(y: T) = {
      val self(a) = x.runtimeChecked
      val self(b) = y.runtimeChecked
      this(a / b)
    }
    override def remainder(y: T) = {
      val self(a) = x.runtimeChecked
      val self(b) = y.runtimeChecked
      this(a % b)
    }
    def divideAndRemainder(y: T) = (x / y, x % y)
  }
  def gcd(x: T, y: T) = {
    val self(a) = x.runtimeChecked
    val self(b) = y.runtimeChecked
    this(ring.gcd(a, b))
  }

  extension (ring: UniqueFactorizationDomain[R]) def apply(s: R*) = {
    same(s*)
    this
  }
}
