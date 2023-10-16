package scas.power.impl

import scas.math.Numeric

trait Lexicographic[N : Numeric] extends ArrayPowerProductWithDegree[N] {
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
