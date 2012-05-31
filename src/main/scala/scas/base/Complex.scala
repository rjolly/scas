package scas.base

import scas.polynomial.tree.UnivariatePolynomial
import scas.Implicits.QQ

object Complex extends scas.polynomial.Complex(UnivariatePolynomial(QQ, "I")) {
  override def toString = "CC"
  override def toMathML = <complexes/>
}
