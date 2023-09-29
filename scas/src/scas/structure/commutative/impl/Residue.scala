package scas.structure.commutative.impl

import scala.annotation.targetName
import scas.base.BigInteger

trait Residue[T](using ring: UniqueFactorizationDomain[T]) extends UniqueFactorizationDomain[T] {
  def apply(x: T): T
  @targetName("fromInt") def apply(n: BigInteger) = this(ring(n))
  override def convert(x: T) = this(ring.convert(x))
  extension (x: T) {
    def signum = ring.signum(x)
    def add(y: T) = this(ring.add(x)(y))
    def subtract(y: T) = this(ring.subtract(x)(y))
    def multiply(y: T) = this(ring.multiply(x)(y))
  }
  def equiv(x: T, y: T) = ring.equiv(x, y)
  extension (x: T) {
    def toCode(level: Level) = ring.toCode(x)(level)
    def toMathML = ring.toMathML(x)
  }
  def zero = ring.zero
  def one = ring.one

  extension (ring: UniqueFactorizationDomain[T]) def apply(s: T*): Residue[T]
}
