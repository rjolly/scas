package scas.base

import scas.structure.{Residue, Field}
import scas.Implicits.{ZZ, infixUFDOps}
import scas.long2bigInteger

class ModInteger(val mod: java.math.BigInteger) extends BigInteger with Residue[java.math.BigInteger] with Field[java.math.BigInteger] {
  assert (mod.isProbablePrime(100))
  def reduce(x: java.math.BigInteger) = x.mod(mod)
  override def apply(l: Long) = l
  override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(new java.math.BigInteger(numbits, rnd))
  override def characteristic = mod
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.modPow(exp, mod)
  def inverse(x: java.math.BigInteger) = x.modInverse(mod)
  override def toString = super.toString + "(" + mod + ")"
  override def toMathML = <msub>{super.toMathML}<mn>{mod}</mn></msub>
}

object ModInteger {
  def apply(mod: java.math.BigInteger) = new ModInteger(mod)
}
