package scas.power

import scas.math.Numeric

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](vs: String*) extends PowerProductWithDegree[N] {
  val variables = vs.toArray
  def compare(x: Array[N], y: Array[N]) = {
    var i = length
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    0
  }
}
