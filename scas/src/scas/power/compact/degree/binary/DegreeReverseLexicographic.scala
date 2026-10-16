package scas.power.compact.degree.binary

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic(variables: Variable*) extends scas.power.compact.degree.DegreeReverseLexicographic(0)(variables*) with PowerProduct {
  def relaxed = new scas.power.compact.degree.DegreeReverseLexicographic(1)(variables*)
}

object DegreeReverseLexicographic {
  def apply[S : Conversion[Variable]](variables: S*) = new DegreeReverseLexicographic(variables.map(~_)*)
}
