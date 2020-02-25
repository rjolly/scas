package scas.base

import scas.structure.Residue
import scas.structure.ordered.Field
import scas.BigInteger

class ModInteger(val mod: BigInteger) extends Residue[BigInteger] with Field[BigInteger] {
  assert (mod.isProbablePrime(100))
  def apply(x: BigInteger) = x.mod(mod)
  def compare(x: BigInteger, y: BigInteger) = BigInteger.compare(x, y)
  override def signum(x: BigInteger) = super[Residue].signum(x)
  def characteristic = mod
  override def (a: BigInteger) \ (b: BigInteger) = a.modPow(b, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
}
