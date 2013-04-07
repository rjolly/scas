package scas.power

import scala.reflect.ClassTag
import scas.Variable
import Ordering.Implicits.infixOrderingOps
import PowerProduct.degreeReverseLexicographic

class DegreeReverseLexicographic[@specialized(Byte, Short, Int, Long) N](val variables: Array[Variable])(implicit val nm: Numeric[N], val m: ClassTag[N], val cm: ClassTag[Array[N]]) extends PowerProduct[N] {
  def self(variables: Array[Variable]) = degreeReverseLexicographic[N](variables)
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = length
    if (x(n) < y(n)) -1
    else if (x(n) > y(n)) 1
    else {
      var i = 0
      while (i < n) {
        if (x(i) > y(i)) return -1
        else if (x(i) < y(i)) return 1
        i += 1
      }
      return 0
    }
  }
}
