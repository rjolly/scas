package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.{ClassTagArray, Conversion, unary_~}
import scas.variable.Variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) extends Lexicographic.Impl[N](variables: _*) with ArrayPowerProductWithDegree[N] {
  given instance: Lexicographic[N] = this
}

object Lexicographic {
  abstract class Impl[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends ArrayPowerProductWithDegree.Impl[N] {
    given instance: Impl[N]
    val self: Impl[N] = this
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

  inline def apply[N : ClassTag : ClassTagArray](using numeric: Numeric[N])(variables: String*): Lexicographic[N] = apply(numeric.fromInt(0))(variables: _*)

  inline def apply[N : ClassTag : ClassTagArray, U: Conversion[Variable]](degree: N)(using numeric: Numeric[N])(variables: U*): Lexicographic[N] = new Lexicographic[N](variables.map(~_): _*) {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (numeric.lt(x(i), y(i))) return -1
        else if (numeric.gt(x(i), y(i))) return 1
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
