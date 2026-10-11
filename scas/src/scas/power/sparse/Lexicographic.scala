package scas.power.sparse

import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic(val variables: Variable*) extends PowerProduct {
  def compare(x: Array[Int], y: Array[Int]) = {
    var i = x.size
    var j = y.size
    while i > 0 && j > 0 do {
      i -= 1
      j -= 1
      if x(i) < y(j) then return -1
      if x(i) > y(j) then return 1
      if x.get(i) < y.get(j) then return -1
      if x.get(i) > y.get(j) then return 1
    }
    if j > 0 then return -1
    if i > 0 then return 1
    0
  }
}

object Lexicographic {
  def apply[S : Conversion[Variable]](variables: S*) = new Lexicographic(variables.map(~_)*)
}
