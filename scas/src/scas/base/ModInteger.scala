package scas.base

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.{Residue, Field}
import BigInteger.given
import ModInteger.Impl

class ModInteger(val mod: BigInteger) extends Impl with Field.Ops[BigInteger] {
  given instance: ModInteger = this
}

object ModInteger {
  abstract class Impl extends Residue[BigInteger] with Field[BigInteger] {
    given instance: Impl
    assert (mod.isProbablePrime(100))
    def mod: BigInteger
    def apply(x: BigInteger) = x.mod(mod)
    def characteristic = mod
    extension (a: BigInteger) override def pow(b: BigInteger) = a.modPow(b, mod)
    def inverse(x: BigInteger) = x.modInverse(mod)
    override def toString = s"ModInteger(\"$mod\")"
    def toMathML = s"<msub>${BigInteger.toMathML}${BigInteger.toMathML(mod)}</msub>"

    extension (ring: UniqueFactorizationDomain[BigInteger]) def apply(str: String): scas.structure.commutative.Residue[BigInteger] = ring(BigInteger(str))

    extension (ring: UniqueFactorizationDomain[BigInteger]) def apply(s: BigInteger*) = {
      assert (s.size == 1 && s(0) >< mod)
      this
    }
  }

  def apply(str: String): Impl = new ModInteger(BigInteger(str))
}
