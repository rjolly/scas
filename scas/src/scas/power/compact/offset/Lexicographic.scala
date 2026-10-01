package scas.power.compact.offset

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic(val shift: Int)(val variables: Variable*) extends PowerProduct {
  def compare(x: Array[Int], n: Int, y: Array[Int], m: Int) = {
    val k = n * length
    val l = m * length
    var i = length + k
    var j = length + l
    while i > k do {
      i -= 1
      j -= 1
      if x(i) < y(j) then return -1
      if x(i) > y(j) then return 1
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
