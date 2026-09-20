package scas.power.compact.degree

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic(val shift: Int)(val variables: Variable*) extends PowerProduct {
  def compare(x: Array[Int], y: Array[Int]) = {
    if x.deg < y.deg then return -1
    if x.deg > y.deg then return 1
    var i = len
    while i > 0 do {
      i -= 1
      if x(i) > y(i) then return -1
      if x(i) < y(i) then return 1
    }
    0
  }
  extension (x: Array[Int]) {
    override def get(i: Int) = super.get(x)(length - 1 - i)
    override def set(i: Int, c: Int) = super.set(x)(length - 1 - i, c)
  }
}

object DegreeReverseLexicographic {
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new DegreeReverseLexicographic(shift)(variables.map(~_)*)

  def binary[S : Conversion[Variable]](variables: S*): BinaryPowerProduct = new DegreeReverseLexicographic(0)(variables.map(~_)*) with BinaryPowerProduct {
    def defining = new DegreeReverseLexicographic(1)(this.variables*)
  }
}
