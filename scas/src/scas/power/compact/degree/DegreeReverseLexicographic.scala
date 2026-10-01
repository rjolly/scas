package scas.power.compact.degree

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic(val shift: Int)(val variables: Variable*) extends ReversePowerProduct {
  def compare(x: Array[Int], y: Array[Int]) = {
    if x.deg < y.deg then return -1
    if x.deg > y.deg then return 1
    var i = length - 1
    while i > 0 do {
      i -= 1
      if x(i) > y(i) then return -1
      if x(i) < y(i) then return 1
    }
    0
  }
}

object DegreeReverseLexicographic {
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new DegreeReverseLexicographic(shift)(variables.map(~_)*)

  def binary[S : Conversion[Variable]](variables: S*): BinaryPowerProduct = new DegreeReverseLexicographic(0)(variables.map(~_)*) with BinaryPowerProduct {
    def defining = new DegreeReverseLexicographic(1)(this.variables*)
  }
}
