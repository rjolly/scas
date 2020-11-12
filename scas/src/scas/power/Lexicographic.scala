package scas.power

import scas.math.Numeric
import scas.variable.Variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends ArrayPowerProductWithDegree[N] {
  def compare(x: IArray[N], y: IArray[N]) = {
    var i = length
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    0
  }
}

object Lexicographic {
  inline def apply[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) = new Lexicographic[N](variables: _*) {
    override def compare(x: IArray[N], y: IArray[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (x(i) < y(i)) return -1
        else if (x(i) > y(i)) return 1
      }
      0
    }
    extension (x: IArray[N]) override def * (y: IArray[N]) = {
      val r = mone
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      IArray.unsafeFromArray(r)
    }
  }
}
