package scas.power.offset.degree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends ArrayPowerProduct[N] {
  def compare(x: Array[N], n: Int, y: Array[N], m: Int) = {
    if x.deg(n) < y.deg(m) then return -1
    if x.deg(n) > y.deg(m) then return 1
    val k = n * length
    val l = m * length
    var i = k
    var j = l
    while i < nbvars do {
      if x(i) > y(j) then return -1
      if x(i) < y(j) then return 1
      i += 1
      j += 1
    }
    0
  }
}
