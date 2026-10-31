package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class KthElimination[N : {Numeric, ClassTag}](k: Int)(val variables: Variable*) extends KthElimination.Impl[N](k)

object KthElimination {
  trait Impl[N : {Numeric, ClassTag}](k: Int) extends ArrayPowerProduct[N] {
    def compare(x: Array[N], y: Array[N]) = {
      var i = nbvars
      while i > k do {
        i -= 1
        if x.get(i) < y.get(i) then return -1
        if x.get(i) > y.get(i) then return 1
      }
      if x.deg < y.deg then return -1
      if x.deg > y.deg then return 1
      i = 0
      while i < k do {
        if x.get(i) > y.get(i) then return -1
        if x.get(i) < y.get(i) then return 1
        i += 1
      }
      0
    }
  }
}
