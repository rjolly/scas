package scas.base

import scas.structure.commutative.Residue
import scas.structure.commutative.ordered.Field
import scala.util.FromDigits
import scas.BigInteger

class ModInteger(val mod: BigInteger) extends Residue(using BigInteger) with Field[BigInteger] with FromDigits[BigInteger] {
  def fromDigits(digits: String) = BigInteger.fromDigits(digits)
  given ModInteger = this
  assert (mod.isProbablePrime(100))
  override def apply(x: BigInteger) = x.mod(mod)
  def compare(x: BigInteger, y: BigInteger) = BigInteger.compare(x, y)
  extension (x: BigInteger) override def signum = super[Residue].signum(x)
  def characteristic = mod
  extension (a: BigInteger) override def pow(b: BigInteger) = a.modPow(b, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
  override def toString = s"ModInteger($mod)"
  def toMathML = s"<msub>${ring.toMathML}${BigInteger.toMathML(mod)}</msub>"
}

object ModInteger
