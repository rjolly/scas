package scas.base

import scas.polynomial.ufd.tree.UnivariatePolynomial
import scas.polynomial.ufd.AlgebraicNumber
import scas.int2bigInteger
import scas.Implicits.QQ

object Complex extends AlgebraicNumber(UnivariatePolynomial(QQ, "I")) {
  implicit val self = this
  val I = generators(0)
  update(pow(I, 2) + 1)
  override def toString = "CC"
  override def toMathML = <complexes/>
}
