package scas.base

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.{Residue, Field}
import ModInteger.{Impl, Ops}
import BigInteger.given

class ModInteger(val mod: BigInteger) extends Impl with Ops {
  given ModInteger = this
}

object ModInteger {
  trait Impl extends Residue[BigInteger] with Field[BigInteger] {
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

  trait Ops extends Field.Ops[BigInteger] { this: Impl =>
  }

  def apply(str: String) = new ModInteger(BigInteger(str))
}
