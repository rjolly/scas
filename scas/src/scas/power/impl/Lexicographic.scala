package scas.power.impl

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.ClassTagArray

trait Lexicographic[N : Numeric : ClassTag : ClassTagArray] extends ArrayPowerProductWithDegree[N] {
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
