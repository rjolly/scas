package scas.base

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.{Field, Residue}
import BigInteger.given

abstract class ModInteger extends Residue[BigInteger] with Field[BigInteger] {
  assert (mod.isProbablePrime(100))
  def mod: BigInteger
  def apply(x: BigInteger) = x.mod(mod)
  def characteristic = mod
  extension (a: BigInteger) override def pow(b: BigInteger) = a.modPow(b, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
  override def toString = s"ModInteger(\"$mod\")"
  def toMathML = s"<msub>${BigInteger.toMathML}${BigInteger.toMathML(mod)}</msub>"

  extension (ring: UniqueFactorizationDomain[BigInteger]) def apply(s: BigInteger*) = {
    assert (s.size == 1 && s(0) >< mod)
    this
  }
}

object ModInteger {
  def apply(str: String) = new conversion.ModInteger(BigInteger(str))
}
