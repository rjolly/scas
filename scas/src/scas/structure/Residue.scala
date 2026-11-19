package scas.structure

import scala.compiletime.deferred
import scas.base.BigInteger

trait Residue[T, R] extends Ring[T] {
  given ring: Ring[R] = deferred
  def apply(x: R): T
  val self = this
  def unapply(x: T): Option[R]
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def fromRing(x: R): T
  extension (x: T) {
    def signum = {
      val self(a) = x.runtimeChecked
      ring.signum(a)
    }
    def add(y: T) = {
      val self(a) = x.runtimeChecked
      val self(b) = y.runtimeChecked
      this(a + b)
    }
    def subtract(y: T) = {
      val self(a) = x.runtimeChecked
      val self(b) = y.runtimeChecked
      this(a - b)
    }
    def multiply(y: T) = {
      val self(a) = x.runtimeChecked
      val self(b) = y.runtimeChecked
      this(a * b)
    }
    def isUnit = {
      val self(a) = x.runtimeChecked
      ring.isUnit(a)
    }
  }
  extension (x: T) {
    def toCode(level: Level) = {
      val self(a) = x.runtimeChecked
      a.toCode(level)
    }
    def toMathML = {
      val self(a) = x.runtimeChecked
      a.toMathML
    }
  }

  extension (ring: Ring[R]) def apply(s: R*) = {
    same(s*)
    this
  }
  def same(s: R*): Unit
}
