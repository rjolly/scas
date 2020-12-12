package scas.power

import scas.math.Numeric
import scas.variable.Variable

object Lexicographic {
  class Factory[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends ArrayPowerProductWithDegree.Factory[N] {
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

  inline def apply[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) = new Factory[N](variables: _*) {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (x(i) < y(i)) return -1
        else if (x(i) > y(i)) return 1
      }
      0
    }
    extension (x: Array[N]) override def * (y: Array[N]) = {
      val r = newArray
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
  }
}
