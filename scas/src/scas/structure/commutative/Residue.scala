package scas.structure.commutative

import scas.base.BigInteger

trait Residue[T, R](using ring: UniqueFactorizationDomain[R]) extends UniqueFactorizationDomain[T] {
  def apply(x: R): T
  def unapply(x: T): Option[R]
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def fromRing(x: R): T
  extension (x: T) {
    def signum = {
      val this(a) = x: @unchecked
      ring.signum(a)
    }
    def add(y: T) = {
      val this(a) = x: @unchecked
      val this(b) = y: @unchecked
      this(a + b)
    }
    def subtract(y: T) = {
      val this(a) = x: @unchecked
      val this(b) = y: @unchecked
      this(a - b)
    }
    def multiply(y: T) = {
      val this(a) = x: @unchecked
      val this(b) = y: @unchecked
      this(a * b)
    }
  }
  def equiv(x: T, y: T) = {
    val this(a) = x: @unchecked
    val this(b) = y: @unchecked
    val this(c) = this(a): @unchecked
    val this(d) = this(b): @unchecked
    c >< d
  }
  extension (x: T) {
    def toCode(level: Level) = {
      val this(a) = x: @unchecked
      a.toCode(level)
    }
    def toMathML = {
      val this(a) = x: @unchecked
      a.toMathML
    }
  }
  def zero = fromRing(ring.zero)
  def one = fromRing(ring.one)

  extension (ring: UniqueFactorizationDomain[R]) def apply(s: R*): Residue[T, R]
}
