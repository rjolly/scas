package scas.structure.commutative

import scala.compiletime.deferred
import scas.base.BigInteger

trait Residue[T, R] extends UniqueFactorizationDomain[T] {
  given ring: () => UniqueFactorizationDomain[R] = deferred
  def apply(x: R): T
  val self = this
  def unapply(x: T): Option[R]
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def fromRing(x: R): T
  extension (x: T) {
    def signum = {
      val self(a) = x: @unchecked
      ring.signum(a)
    }
    def add(y: T) = {
      val self(a) = x: @unchecked
      val self(b) = y: @unchecked
      this(a + b)
    }
    def subtract(y: T) = {
      val self(a) = x: @unchecked
      val self(b) = y: @unchecked
      this(a - b)
    }
    def multiply(y: T) = {
      val self(a) = x: @unchecked
      val self(b) = y: @unchecked
      this(a * b)
    }
    def isUnit = {
      val self(a) = x: @unchecked
      ring.isUnit(a)
    }
    override def convert = {
      val self(a) = x: @unchecked
      this(a.convert)
    }
    override def divide(y: T) = {
      val self(a) = x: @unchecked
      val self(b) = y: @unchecked
      this(a / b)
    }
    override def remainder(y: T) = {
      val self(a) = x: @unchecked
      val self(b) = y: @unchecked
      this(a % b)
    }
    def divideAndRemainder(y: T) = (x / y, x % y)
  }
  def gcd(x: T, y: T) = {
    val self(a) = x: @unchecked
    val self(b) = y: @unchecked
    this(ring.gcd(a, b))
  }
  def equiv(x: T, y: T) = {
    val self(a) = x: @unchecked
    val self(b) = y: @unchecked
    val self(c) = this(a): @unchecked
    val self(d) = this(b): @unchecked
    c >< d
  }
  extension (x: T) {
    def toCode(level: Level) = {
      val self(a) = x: @unchecked
      a.toCode(level)
    }
    def toMathML = {
      val self(a) = x: @unchecked
      a.toMathML
    }
  }

  extension (ring: UniqueFactorizationDomain[R]) def apply(s: R*) = {
    same(s*)
    this
  }
  def same(s: R*): Unit
}
