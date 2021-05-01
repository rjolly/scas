package scas.power.conversion

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.{ClassTagArray, Conversion, unary_~}
import scas.variable.Variable
import Variable.string2variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) extends scas.power.Lexicographic[N](variables: _*) with PowerProduct[Array[N]]

object Lexicographic {
  def apply[N : Numeric : ClassTag : ClassTagArray, U: Conversion[Variable]](degree: N)(variables: U*) = new Lexicographic[N](variables.map(~_): _*)

  inline def apply[N : ClassTag : ClassTagArray](using numeric: Numeric[N])(variables: String*) = new Lexicographic[N](variables.map(string2variable): _*) {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (numeric.<(x(i))(y(i))) return -1
        else if (numeric.>(x(i))(y(i))) return 1
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
