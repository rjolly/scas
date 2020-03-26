package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric

class Lexicographic[N : Numeric : ClassTag](variables: Array[String]) extends PowerProductWithDegree[N](variables) {
  def compares(x: Array[N], y: Array[N]) = {
    var i = length
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    0
  }
}
