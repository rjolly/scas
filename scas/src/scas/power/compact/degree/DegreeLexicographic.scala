package scas.power.compact.degree

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeLexicographic(val shift: Int)(val variables: Variable*) extends PowerProduct {
  def compare(x: Array[Int], y: Array[Int]) = {
    if x.deg < y.deg then return -1
    if x.deg > y.deg then return 1
    var i = len
    while i > 0 do {
      i -= 1
      if x(i) < y(i) then return -1
      if x(i) > y(i) then return 1
    }
    0
  }
}

object DegreeLexicographic {
  def apply[S : Conversion[Variable]](shift: Int)(variables: S*) = new DegreeLexicographic(shift)(variables.map(~_)*)
}
