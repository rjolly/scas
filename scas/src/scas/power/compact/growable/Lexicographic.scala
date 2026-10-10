package scas.power.compact.growable

import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.power.compact.Lexicographic.Impl

class Lexicographic(shift: Int)(variables: Variable*) extends PowerProduct(shift)(variables*) with Impl

object Lexicographic {
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new Lexicographic(shift)(variables.map(~_)*)

  def binary[S : Conversion[Variable]](variables: S*): BinaryPowerProduct = new BinaryPowerProduct(0)(variables.map(~_)*) with Impl {
    def defining = new Lexicographic(1)(this.variables*)
  }
}
