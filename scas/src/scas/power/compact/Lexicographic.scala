package scas.power.compact

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic(val shift: Int)(val variables: Variable*) extends PowerProduct {
  def compare(x: Array[Int], y: Array[Int]) = {
    var i = len
    while i > 0 do {
      i -= 1
      if x(i) < y(i) then return -1
      if x(i) > y(i) then return 1
    }
    0
  }
}

object Lexicographic {
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new Lexicographic(shift)(variables.map(~_)*)

  def binary[S : Conversion[Variable]](variables: S*): BinaryPowerProduct = new Lexicographic(0)(variables.map(~_)*) with BinaryPowerProduct {
    def defining = new Lexicographic(1)(this.variables*)
  }
}
