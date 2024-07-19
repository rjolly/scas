package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class Lexicographic[N : Numeric : ClassTag](val variables: Variable*) extends ArrayPowerProductWithDegree[N] {
  def newInstance(variables: Variable*) = new Lexicographic[N](variables*)

  def compare(x: Array[N], y: Array[N]) = {
    var i = length
    while (i > 0) {
      i -= 1
      if (x(i) < y(i)) return -1
      if (x(i) > y(i)) return 1
    }
    0
  }
}

object Lexicographic {
  def apply[N : Numeric : ClassTag, S : Conversion[Variable]](degree: N)(variables: S*) = new conversion.Lexicographic[N](variables.map(~_)*)

  inline def inlined[N : Numeric : ClassTag, S : Conversion[Variable]](degree: N)(variables: S*): Lexicographic[N] = new Lexicographic[N](variables.map(~_)*) {
    override def newInstance(variables: Variable*) = ???

    override def compare(x: Array[N], y: Array[N]) = {
      var i = length
      while (i > 0) {
        i -= 1
        if (x(i) < y(i)) return -1
        if (x(i) > y(i)) return 1
      }
      0
    }
    extension (x: Array[N]) override def multiply(y: Array[N]) = {
      val r = empty
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
  }
}
