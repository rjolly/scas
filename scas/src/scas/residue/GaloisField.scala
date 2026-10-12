package scas.residue

import scas.variable.Variable
import scas.base.ModInteger

class GaloisField(str: String)(variables: Variable*) extends AlgebraicNumber(ModInteger(str))(variables*) {
  given instance: AlgebraicNumber[Int] = this
}
