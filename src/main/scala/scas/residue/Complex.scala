package scas.residue

import scas.structure.StarRingWithUFD
import scas.{Variable, BigInteger, Rational, UnivariatePolynomial, int2bigInteger, bigInteger2rational}
import scas.Implicits.{ZZ, QQ, coef2residue}

trait ComplexLike extends AlgebraicNumber[UnivariatePolynomial.Element[Rational, Int], Rational, Int] with StarRingWithUFD[Complex] {
  val ring = UnivariatePolynomial(QQ, Variable.sqrt(BigInteger(-1)))
  import ring.pp
  implicit val self = this
  update(1 + pow(sqrt(-1), 2))
  def sqrt(x: Complex) = { assert (x >< -1) ; generator(0) }
  def real(x: Complex) = coefficient(x, pp.one)
  def imag(x: Complex) = coefficient(x, pp.generator(0))
  def isReal(x: Complex) = imag(x) >< zero
  def isImag(x: Complex) = real(x) >< zero
  def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
  override def toString = "CC"
  override def toMathML = <complexes/>
}
