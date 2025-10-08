package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends DegreeReverseLexicographic.Impl[N]

object DegreeReverseLexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new DegreeReverseLexicographic[N](variables.map(~_)*)

  trait Impl[N : {Numeric, ClassTag}] extends ArrayPowerProduct[N] {
    def compare(x: Array[N], y: Array[N]) = {
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      var i = 0
      while i < length do {
        if x.get(i) > y.get(i) then return -1
        if x.get(i) < y.get(i) then return 1
        i += 1
      }
      0
    }
  }
}
