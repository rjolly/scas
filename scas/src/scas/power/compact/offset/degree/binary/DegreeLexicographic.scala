package scas.power.compact.offset.degree.binary

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeLexicographic(variables: Variable*) extends scas.power.compact.offset.degree.DegreeLexicographic(0)(variables*) with PowerProduct {
  def relaxed = new scas.power.compact.offset.degree.DegreeLexicographic(1)(variables*)
}

object DegreeLexicographic {
  def apply[S : Conversion[Variable]](variables: S*) = new DegreeLexicographic(variables.map(~_)*)
}
