package scas.power.sparse

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends DegreeReverseLexicographic.Impl[N]

object DegreeReverseLexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new DegreeReverseLexicographic[N](variables.map(~_)*)

  trait Impl[N : {Numeric, ClassTag}] extends PowerProduct[N] {
    def compare(x: Array[N], y: Array[N]) = {
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      var i = 0
      var j = 0
      while i < x.size && j < y.size do {
        if x(i) < y(j) then return -1
        if x(i) > y(j) then return 1
        if x.get(i) > y.get(j) then return -1
        if x.get(i) < y.get(j) then return 1
        i += 1
        j += 1
      }
      if i < x.size then return -1
      if j < y.size then return 1
      0
    }
  }
}
