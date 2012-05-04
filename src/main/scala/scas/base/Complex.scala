package scas.base

import scas.polynomial.AlgebraicNumber
import scas.{int2bigInteger, bigInteger2rational, UnivariatePolynomial}
import scas.Implicits.QQ

object Complex extends AlgebraicNumber(UnivariatePolynomial(QQ, "I")) {
  implicit val self = this
  val I = generators(0)
  update(1 + pow(I, 2))
  override def toString = "CC"
  override def toMathML = <complexes/>
}
