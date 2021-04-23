package scas.base

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.{Residue, Field}
import BigInteger.given

class ModInteger(val mod: BigInteger) extends Residue[BigInteger] with Field[BigInteger] {
  assert (mod.isProbablePrime(100))
  override def apply(x: BigInteger) = x.mod(mod)
  def characteristic = mod
  extension (a: BigInteger) override def pow(b: BigInteger) = a.modPow(b, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
  override def toString = s"$BigInteger.residue($mod)"
  def toMathML = s"<msub>${BigInteger.toMathML}${BigInteger.toMathML(mod)}</msub>"

  extension (ring: UniqueFactorizationDomain[BigInteger]) def residue(s: BigInteger*) = {
    assert (s.size == 1 && s(0) >< mod)
    this
  }
}
