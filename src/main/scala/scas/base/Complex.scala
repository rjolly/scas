package scas.base

import scas.structure.{Field, StarRing}
import scas.{int2bigInteger, double2complex}

trait ComplexLike extends StarRing[Complex] with Field[Complex] {
  def convert(x: Complex) = x
  def apply(a: Double, b: Double) = (a, b)
  def apply(value: Double): Complex = apply(value, 0)
  def apply(l: Long) = apply(l.toDouble)
  def signum(x: Complex) = if (x.isImag) signum(imag(x)) else signum(real(x))
  def random(numbits: Int)(implicit rnd: java.util.Random) = (rnd.nextDouble(), rnd.nextDouble())
  def characteristic = 0
  override def pow(x: Complex, exp: BigInteger): Complex = pow(x, exp.doubleValue())
  def gcd(x: Complex, y: Complex) = if (norm(x) < norm(y)) y else x
  def plus(x: Complex, y: Complex) = (real(x) + real(y), imag(x) + imag(y))
  def minus(x: Complex, y: Complex) = (real(x) - real(y), imag(x) - imag(y))
  def times(x: Complex, y: Complex) = (real(x) * real(y) - imag(x) * imag(y), real(x) * imag(y) + real(y) * imag(x))
  def divide(x: Complex, y: Double) = (real(x) / y, imag(x) / y)
  def inverse(x: Complex) = divide(conjugate(x), magnitude2(x))

  def sqrt(x: Complex) = { assert (x >< -1) ; apply(0, 1) }
  def real(x: Complex) = { val (a, _) = x ; a }
  def imag(x: Complex) = { val (_, b) = x ; b }
  def isReal(x: Complex) = imag(x) == 0
  def isImag(x: Complex) = real(x) == 0

  def conjugate(x: Complex) = (real(x), -imag(x))
  def magnitude2(x: Complex) = Math.pow(real(x), 2) + Math.pow(imag(x), 2)
  def magnitude(x: Complex) = Math.sqrt(magnitude2(x))
  def pow(x: Complex, y: Complex) = apply(Math.pow(real(x), real(y)), 0)

  def equiv(x: Complex, y: Complex) = real(x) == real(y) && imag(x) == imag(y)
  override def toString = "CC"
  def toMathML(x: Complex) = <cn type="complex">{real(x)}<sep/>{imag(x)}</cn>
  def toMathML = <complexes/>
}
