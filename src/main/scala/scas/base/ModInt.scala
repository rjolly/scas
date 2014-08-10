package scas.base

import scas.structure.Residue
import scas.structure.Field
import scas.int2bigInteger

class ModInt(val mod: Int) extends Residue[Int, Long] with Field[Int] {
  val self = this
  val ring = Long
  assert (mod.isProbablePrime(100))
  def fromRing(value: Long) = value.toInt
  def reduce(value: Long) = fromRing(value % mod)
  def unapply(x: Int) = Some(x.toLong)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(new BigInteger(numbits, rnd).longValue)
  override def isZero(x: Int) = x == 0
  def characteristic = mod
  override def plus(x: Int, y: Int) = {
    val a = x.toLong
    val b = y.toLong
    reduce(a + b)
  }
  override def minus(x: Int, y: Int) = {
    val a = x.toLong
    val b = y.toLong
    reduce(a - b)
  }
  override def times(x: Int, y: Int) = {
    val a = x.toLong
    val b = y.toLong
    reduce(a * b)
  }
  override def pow(x: Int, exp: BigInteger) = x.modPow(exp, mod).intValue
  def inverse(x: Int) = x.modInverse(mod).intValue
  override def toString = ring.toString + "(" + mod + ")"
  def toMathML = <msub>{ring.toMathML}<mn>{mod}</mn></msub>
}

object ModInt {
  def apply(mod: Int) = new ModInt(mod)
}
