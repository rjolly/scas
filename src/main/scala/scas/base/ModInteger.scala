package scas.base

import scas.structure.Residue
import scas.structure.ordered.Field
import scas.Implicits.ZZ

class ModInteger(val mod: BigInteger) extends Residue[BigInteger, BigInteger] with Field[BigInteger] {
  val ring = ZZ
  assert (mod.isProbablePrime(100))
  def fromRing(value: BigInteger) = value
  def reduce(value: BigInteger) = value.mod(mod)
  def unapply(x: BigInteger) = Some(x)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(new BigInteger(numbits, rnd))
  def characteristic = mod
  override def pow(x: BigInteger, exp: BigInteger) = x.modPow(exp, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
  def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  override def toString = ring.toString + "(" + mod + ")"
  def toMathML = <msub>{ring.toMathML}<mn>{mod}</mn></msub>
}

object ModInteger {
  def apply(mod: BigInteger) = new ModInteger(mod)
}
