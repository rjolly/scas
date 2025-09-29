package scas.base

import scas.structure.commutative.ordered.{Residue, Field}
import scas.module.ArrayModule
import BigInteger.given

class ModInteger(mod: BigInteger) extends Residue[Int, BigInteger] with Field[Int] {
  assert (mod.isProbablePrime(100))
  override given ring: BigInteger.type = BigInteger
  def apply(x: BigInteger) = this(x.longValue)
  def unapply(x: Int) = Some(int2bigInt(x))
  def fromRing(x: BigInteger) = this(x)
  def characteristic = mod
  val m = mod.intValue
  def apply(x: Long) = {
      val c = (x % m).toInt
      if c < 0 then c + m else c
  }
  extension (x: Int) {
    override def signum = java.lang.Integer.signum(x)
    override def add(y: Int) = this(x.toLong + y)
    override def subtract(y: Int) = this(x.toLong - y)
    override def multiply(y: Int) = this(x.toLong * y)
    override def isZero = x.signum == 0
  }
  extension (a: Int) override def pow(b: BigInteger) = int2bigInt(a).modPow(b, mod).intValue
  def inverse(x: Int) = int2bigInt(x).modInverse(mod).intValue
  override def toString = s"ModInteger(\"${mod}\")"
  def toMathML = s"<msub>${BigInteger.toMathML}${mod.toMathML}</msub>"

  extension (ring: BigInteger.Impl) def apply(s: BigInteger*) = {
    same(s*)
    this
  }
  def same(s: BigInteger*): Unit = {
    given ArrayModule[BigInteger] = ArrayModule(BigInteger)(1)
    assert (s.toArray >< List(mod).toArray)
  }
  override val zero = 0
  override val one = 1
}

object ModInteger {
  def apply(str: String) = new Conv(BigInteger(str))
  class Conv(mod: BigInteger) extends ModInteger(mod) with Field.Conv[Int] {
    given instance: Conv = this
  }
}
