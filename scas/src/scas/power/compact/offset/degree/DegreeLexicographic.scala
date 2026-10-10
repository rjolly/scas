package scas.power.compact.offset.degree

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeLexicographic(val shift: Int)(val variables: Variable*) extends PowerProduct {
  def compare(x: Array[Int], n: Int, y: Array[Int], m: Int) = {
    if x.deg(n) < y.deg(m) then return -1
    if x.deg(n) > y.deg(m) then return 1
    val k = n * length
    val l = m * length
    var i = length - 1 + k
    var j = length - 1 + l
    while i > 0 do {
      i -= 1
      j -= 1
      if x(i) < y(j) then return -1
      if x(i) > y(j) then return 1
    }
    0
  }
}

object DegreeLexicographic {
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new DegreeLexicographic(shift)(variables.map(~_)*)

  def binary[S : Conversion[Variable]](variables: S*): BinaryPowerProduct = new DegreeLexicographic(0)(variables.map(~_)*) with BinaryPowerProduct {
    def relaxed = new DegreeLexicographic(1)(this.variables*)
  }
}
