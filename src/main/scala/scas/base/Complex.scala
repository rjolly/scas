package scas.base

import scas.polynomial.ufd.tree.int.AlgebraicNumber
import scas.int2bigInteger
import scas.Implicits.QQ

object Complex extends AlgebraicNumber(QQ, "I") {
  implicit val self = this
  val I = generator
  update(pow(I, 2) + 1)
}
