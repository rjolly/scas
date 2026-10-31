package scas.power.degree

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class KthElimination[N : {Numeric, ClassTag}](k: Int)(val variables: Variable*) extends ArrayPowerProduct[N] with scas.power.KthElimination.Impl[N](k)

object KthElimination {
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(k: Int)(variables: S*): KthElimination[N] = new KthElimination[N](k)(variables.map(~_)*) {
    override def compare(x: Array[N], y: Array[N]) = {
      var i = nbvars
      while i > k do {
        i -= 1
        if x(i) < y(i) then return -1
        if x(i) > y(i) then return 1
      }
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      i = 0
      while i < k do {
        if x(i) > y(i) then return -1
        if x(i) < y(i) then return 1
        i += 1
      }
      0
    }
    override def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
      var i = 0
      while i <= nbvars do {
        z(i) = x(i) + y(i)
        i += 1
      }
      z
    }
    extension (x: Array[N]) {
      override def deg = x(nbvars)
    }
  }
}
