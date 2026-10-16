package scas.power.compact.offset.binary

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic(variables: Variable*) extends scas.power.compact.offset.Lexicographic(0)(variables*) with PowerProduct {
  def relaxed = new scas.power.compact.offset.Lexicographic(1)(variables*)
}

object Lexicographic {
  def apply[S : Conversion[Variable]](variables: S*) = new Lexicographic(variables.map(~_)*)
}
