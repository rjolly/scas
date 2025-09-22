package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class DegreeLexicographic[N : Numeric : ClassTag](val variables: Variable*) extends ArrayPowerProductWithDegree[N] {
  def newInstance(variables: Variable*) = new DegreeLexicographic[N](variables*)

  def compare(x: Array[N], y: Array[N]) = {
    var i = length + 1
    while i > 0 do {
      i -= 1
      if x(i) < y(i) then return -1
      if x(i) > y(i) then return 1
    }
    0
  }
}
