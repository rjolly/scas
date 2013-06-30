package scas.base

import scas.structure.{Residue, Field}
import scas.int2bigInteger

class ModInt(val mod: Int) extends Residue[Int, Long] with Field[Int] {
  val ring = Long
  assert (mod.isProbablePrime(100))
  def fromRing(value: Long) = value.toInt
  def reduce(value: Long) = fromRing(value % mod)
  def unapply(x: Int) = Some(x.toLong)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(new BigInteger(numbits, rnd).longValue)
  def characteristic = mod
  override def pow(x: Int, exp: BigInteger) = x.modPow(exp, mod).intValue
  def inverse(x: Int) = x.modInverse(mod).intValue
  override def toString = ring.toString + "(" + mod + ")"
  def toMathML = <msub>{ring.toMathML}<mn>{mod}</mn></msub>
}

object ModInt {
  def apply(mod: Int) = new ModInt(mod)
}
