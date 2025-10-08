package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class DegreeLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends DegreeLexicographic.Impl[N]

object DegreeLexicographic {
  trait Impl[N : {Numeric, ClassTag}] extends ArrayPowerProductWithDegree[N] {
    def compare(x: Array[N], y: Array[N]) = {
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      var i = length
      while i > 0 do {
        i -= 1
        if x.get(i) < y.get(i) then return -1
        if x.get(i) > y.get(i) then return 1
      }
      0
    }
  }
}
