package scas.base

import scas.structure.{BooleanAlgebra, Field}
import scas.Implicits.{ZZ, infixAbelianGroupOps}
import scas.{int2bigInteger, Variable}

object Boolean extends BooleanAlgebra[Boolean] with Field[Boolean] {
  def apply(l: Long) = l % 2 == 1
  def random(numbits: Int)(implicit rnd: java.util.Random) = rnd.nextBoolean()
  def characteristic = 2
  override def pow(x: Boolean, exp: BigInteger) = if (BigInteger.signum(exp) < 0) pow(inverse(x), -exp) else if (BigInteger.signum(exp) == 0) true else x
  def signum(x: Boolean) = if(x) 1 else 0
  def plus(x: Boolean, y: Boolean) = x ^ y
  def minus(x: Boolean, y: Boolean) = x + y
  def times(x: Boolean, y: Boolean) = x && y
  def inverse(x: Boolean) = apply(1 / signum(x))
  def equiv(x: Boolean, y: Boolean) = x == y
  override def toString = "ZZ(2)"
  def toMathML(x: Boolean) = if (x) <true/> else <false/>
  def toMathML = <msub><integers/><mn>2</mn></msub>
  def function(x: Boolean, a: Variable) = Function(signum(x).toDouble)
}
