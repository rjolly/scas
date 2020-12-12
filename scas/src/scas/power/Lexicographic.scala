package scas.power

import scas.math.Numeric
import scas.variable.Variable
import ArrayPowerProduct.Element

object Lexicographic {
  class Factory[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends ArrayPowerProductWithDegree.Factory[N] {
    def compare(x: Element[N], y: Element[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (x(i) < y(i)) return -1
        else if (x(i) > y(i)) return 1
      }
      0
    }
  }

  inline def from[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) = new Factory[N](variables: _*) {
    override def compare(x: Element[N], y: Element[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (x(i) < y(i)) return -1
        else if (x(i) > y(i)) return 1
      }
      0
    }
    extension (x: Element[N]) override def * (y: Element[N]) = {
      val r = newArray
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      this(r)
    }
  }
}
