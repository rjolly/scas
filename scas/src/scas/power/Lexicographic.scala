package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.ClassTagArray
import scas.variable.Variable

trait Lexicographic[N : ClassTag : ClassTagArray](using numeric: Numeric[N]) extends ArrayPowerProductWithDegree[N] {
  def compare(x: Array[N], y: Array[N]) = {
    import numeric.{<, >}
    var i = length
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    0
  }
}
