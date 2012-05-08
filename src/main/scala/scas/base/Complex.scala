package scas.base

import scas.polynomial.PowerProduct
import scas.polynomial.tree.AlgebraicNumber
import scas.{int2bigInteger, bigInteger2rational}
import scas.Implicits.QQ

object Complex extends AlgebraicNumber(QQ, PowerProduct('I)) {
  implicit val self = this
  val I = generators(0)
  update(1 + pow(I, 2))
  override def toString = "CC"
  override def toMathML = <complexes/>
}
