package scas.power.degree

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends scas.power.DegreeReverseLexicographic.Impl[N] with ArrayPowerProduct[N]

object DegreeReverseLexicographic {
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site") inline def inlined[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*): DegreeReverseLexicographic[N] = new DegreeReverseLexicographic[N](variables.map(~_)*) {
    override def compare(x: Array[N], y: Array[N]) = {
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      var i = 0
      while i < nbvars do {
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
