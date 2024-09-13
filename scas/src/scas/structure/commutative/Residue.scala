package scas.structure.commutative

import scas.base.BigInteger

trait Residue[T, R] extends UniqueFactorizationDomain[T] {
  given ring: UniqueFactorizationDomain[R]
  def apply(x: R): T
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def fromRing(x: R): T
  extension (x: T) {
    def unapply: R
    def signum = {
      val a = x.unapply
      ring.signum(a)
    }
    def add(y: T) = {
      val a = x.unapply
      val b = y.unapply
      this(a + b)
    }
    def subtract(y: T) = {
      val a = x.unapply
      val b = y.unapply
      this(a - b)
    }
    def multiply(y: T) = {
      val a = x.unapply
      val b = y.unapply
      this(a * b)
    }
    def isUnit = ring.isUnit(x.unapply)
    override def convert = this(x.unapply.convert)
    override def divide(y: T) = {
      val a = x.unapply
      val b = y.unapply
      this(a / b)
    }
    override def remainder(y: T) = {
      val a = x.unapply
      val b = y.unapply
      this(a % b)
    }
    def divideAndRemainder(y: T) = (x / y, x % y)
  }
  def gcd(x: T, y: T) = {
    val a = x.unapply
    val b = y.unapply
    this(ring.gcd(a, b))
  }
  def equiv(x: T, y: T) = {
    val a = x.unapply
    val b = y.unapply
    val c = this(a).unapply
    val d = this(b).unapply
    c >< d
  }
  extension (x: T) {
    def toCode(level: Level) = {
      val a = x.unapply
      a.toCode(level)
    }
    def toMathML = {
      val a = x.unapply
      a.toMathML
    }
  }

  extension (ring: UniqueFactorizationDomain[R]) def apply(s: R*) = {
    same(s*)
    this
  }
  def same(s: R*): Unit
}
