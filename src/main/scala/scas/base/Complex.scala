package scas.base

import scas.UnivariatePolynomial
import scas.{int2bigInteger, bigInteger2rational}
import scas.Implicits.{QQ, infixOps}

object Complex extends scas.polynomial.residue.Complex(UnivariatePolynomial(QQ, "sqrt(-1)")) {
  override def toString = "CC"
  override def toMathML = <complexes/>
}
