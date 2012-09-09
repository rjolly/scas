package scas.base

import scas.polynomial.residue.AlgebraicNumber
import scas.{Variable, UnivariatePolynomial, int2bigInteger, bigInteger2rational}
import scas.Implicits.{ZZ, QQ}

object Complex extends AlgebraicNumber[UnivariatePolynomial.Element[Rational, Int], Rational, Int] {
  val ring = UnivariatePolynomial(QQ, Variable.sqrt(BigInteger(-1)))
  implicit val r = this
  val i = r.generator(0)
  update(1 + pow(i, 2))
  def sqrt(x: Complex) = { assert (x >< -1) ; i }
  override def toString = "CC"
  override def toMathML = <complexes/>
}
