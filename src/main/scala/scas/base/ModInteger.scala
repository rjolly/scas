package scas.base

import scas.structure.{Residue, Field}
import scas.long2bigInteger
import scas.Implicits.{ZZ, infixUFDOps}

class ModInteger(val mod: BigInteger) extends Residue[BigInteger, BigInteger] with Field[BigInteger] {
  val ring = ZZ
  assert (mod.isProbablePrime(100))
  def apply(value: BigInteger) = value
  def reduce(value: BigInteger) = value.mod(mod)
  def unapply(x: BigInteger) = Some(x)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(new BigInteger(numbits, rnd))
  def characteristic = mod
  override def pow(x: BigInteger, exp: BigInteger) = x.modPow(exp, mod)
  def inverse(x: BigInteger) = x.modInverse(mod)
  override def toString = ring.toString + "(" + mod + ")"
  def toMathML = <msub>{ring.toMathML}<mn>{mod}</mn></msub>
}

object ModInteger {
  def apply(mod: BigInteger) = new ModInteger(mod)
}
