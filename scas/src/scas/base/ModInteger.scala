package scas.base

import scas.structure.commutative.ordered.conversion.{Residue, Field}

class ModInteger(val mod: BigInteger) extends Residue(using BigInteger) with Field[BigInteger] {
  given ModInteger = this
  assert (mod.isProbablePrime(100))
  override def apply(x: BigInteger) = x.mod(mod)
  def characteristic = mod
  extension (a: BigInteger) override def pow(b: BigInteger) = a.modPow(b, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
  override def toString = s"ModInteger($mod)"
  def toMathML = s"<msub>${BigInteger.toMathML}${BigInteger.toMathML(mod)}</msub>"
}

object ModInteger {
  def apply[U](x: U)(using c: U => BigInteger) = new ModInteger(c(x))
}
