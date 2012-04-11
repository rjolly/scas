package scas.base

import scas.structure.{Residue, Field}
import scas.Implicits.{ZZ, infixUFDOps}
import scas.long2bigInteger

class ModInteger(val mod: java.math.BigInteger) extends Residue[java.math.BigInteger, java.math.BigInteger] with Field[java.math.BigInteger] {
  assert (mod.isProbablePrime(100))
  def fromRing(x: java.math.BigInteger) = x.mod(mod)
  def toRing(x: java.math.BigInteger) = x
  override def apply(l: Long) = l
  override def random(numbits: Int)(implicit rnd: java.util.Random) = fromRing(new java.math.BigInteger(numbits, rnd))
  def characteristic = mod
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.modPow(exp, mod)
  def divide(x: java.math.BigInteger, y: java.math.BigInteger) = x * y.modInverse(mod)
  override def toString = ring.toString + "(" + mod + ")"
}

object ModInteger {
  def apply(mod: java.math.BigInteger) = new ModInteger(mod)
}
