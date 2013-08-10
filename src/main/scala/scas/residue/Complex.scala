package scas.residue

import scas.structure.StarRing
import scas.{Variable, BigInteger, Rational, UnivariatePolynomial, int2bigInteger, bigInteger2rational}
import scas.Implicits.{ZZ, QQ, coef2residue}

trait ComplexLike extends AlgebraicNumber[UnivariatePolynomial.Element[Rational, Int], Rational, Int] with StarRing[Complex] {
  val ring = UnivariatePolynomial(QQ, Variable.sqrt(BigInteger(-1)))
  import ring.pp
  implicit val self = this
  val i = generator(0)
  update(1 + pow(i, 2))
  def sqrt(x: Complex) = { assert (x >< -1) ; i }
  def real(x: Complex) = coefficient(x, pp.one)
  def imag(x: Complex) = coefficient(x, pp.generator(0))
  def isReal(x: Complex) = imag(x) >< zero
  def isImag(x: Complex) = real(x) >< zero
  def conjugate(x: Complex) = real(x) - sqrt(-1) * imag(x)
  override def toString = "CC"
  override def toMathML = <complexes/>
}
