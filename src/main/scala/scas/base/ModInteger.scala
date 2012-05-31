package scas.base

import scas.structure.{Residue, Field}
import scas.Implicits.{ZZ, infixUFDOps}
import scas.long2bigInteger

class ModInteger(val mod: java.math.BigInteger) extends Residue[java.math.BigInteger, java.math.BigInteger] with Field[java.math.BigInteger] {
  val ring = ZZ
  assert (mod.isProbablePrime(100))
  def apply(value: java.math.BigInteger) = value
  def reduce(value: java.math.BigInteger) = value.mod(mod)
  def unapply(x: java.math.BigInteger) = Some(x)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(new java.math.BigInteger(numbits, rnd))
  def characteristic = mod
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.modPow(exp, mod)
  def inverse(x: java.math.BigInteger) = x.modInverse(mod)
  override def toString = ring.toString + "(" + mod + ")"
  def toMathML = <msub>{ring.toMathML}<mn>{mod}</mn></msub>
}

object ModInteger {
  def apply(mod: java.math.BigInteger) = new ModInteger(mod)
}
