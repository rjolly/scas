package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.{ClassTagArray, unary_~}
import scas.variable.Variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends impl.Lexicographic[N] with PowerProduct[Array[N]] {
  given instance: Lexicographic[N] = this
}

object Lexicographic {
  def apply[N : Numeric : ClassTag : ClassTagArray](degree: N)(s: String*) = new Lexicographic[N](s.map(~_): _*)

  class Impl[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends impl.Lexicographic[N] {
    given instance: Impl[N] = this
  }

  inline def apply[N : Numeric : ClassTag : ClassTagArray](s: String*): Impl[N] = new Impl[N](s.map(~_): _*) {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (x(i) < y(i)) return -1
        else if (x(i) > y(i)) return 1
      }
      0
    }
    extension (x: Array[N]) override def multiply(y: Array[N]) = {
      val r = one
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
  }
}
