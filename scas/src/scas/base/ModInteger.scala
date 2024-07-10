package scas.base

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.{Residue, Field}
import scas.module.ArrayModule
import BigInteger.given

class ModInteger(mod: BigInteger) extends Residue[Int, BigInteger] with Field[Int] {
  assert (mod.isProbablePrime(100))
  def apply(x: BigInteger) = this(x.longValue)
  def fromRing(x: BigInteger) = x.intValue
  def characteristic = mod
  val m = mod.intValue
  def apply(x: Long) = {
      val c = (x % m).toInt
      if (c < 0) c + m else c
  }
  extension (x: Int) {
    def unapply = int2bigInt(x)
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

  extension (ring: UniqueFactorizationDomain[BigInteger]) def apply(s: BigInteger*) = {
    given ArrayModule[BigInteger] = new ArrayModule(using BigInteger)(1)
    assert (s.toArray >< List(mod).toArray)
    this
  }
}

object ModInteger {
  def apply(str: String) = new conversion.ModInteger(BigInteger(str))
}
