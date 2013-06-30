package scas.residue

import scas.{Variable, BigInteger, Rational, UnivariatePolynomial, int2bigInteger, bigInteger2rational}
import scas.Implicits.{ZZ, QQ, coef2residue}

trait ComplexLike extends AlgebraicNumber[UnivariatePolynomial.Element[Rational, Int], Rational, Int] {
  val ring = UnivariatePolynomial(QQ, Variable.sqrt(BigInteger(-1)))
  import ring.pp
  override implicit val self = this
  val i = generator(0)
  update(1 + pow(i, 2))
  def sqrt(x: Complex) = { assert (x >< -1) ; i }
  def realPart(x: Complex) = coefficient(x, pp.one)
  def imaginaryPart(x: Complex) = coefficient(x, pp.generator(0))
  def conjugate(x: Complex) = realPart(x) - i * imaginaryPart(x)
  override def toString = "CC"
  override def toMathML = <complexes/>
}
