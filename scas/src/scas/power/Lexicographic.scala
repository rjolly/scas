package scas.power

import scala.annotation.targetName
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import Variable.string2variable

abstract class Lexicographic[N : Numeric : ClassTag](val variables: Variable*) extends ArrayPowerProductWithDegree[N] {
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
  def apply[N : Numeric : ClassTag](degree: N)(variables: Variable*) = new conversion.Lexicographic[N](variables*)

  @targetName("applyString") def apply[N : Numeric : ClassTag](degree: N)(s: String*): conversion.Lexicographic[N] = this(degree)(s.map(string2variable)*)

  @targetName("applyString") inline def apply[N : Numeric : ClassTag](s: String*): Lexicographic[N] = this(s.map(string2variable)*)

  inline def apply[N : Numeric : ClassTag](variables: Variable*): Lexicographic[N] = new Lexicographic[N](variables*) {
    def newInstance(variables: Variable*) = ???

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
