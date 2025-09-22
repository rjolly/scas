package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](val variables: Variable*) extends ArrayPowerProductWithDegree[N] {
  def newInstance(variables: Variable*) = new DegreeReverseLexicographic[N](variables*)

  def compare(x: Array[N], y: Array[N]) = {
    if x(length) < y(length) then return -1
    if x(length) > y(length) then return 1
    var i = 0
    while i < length do {
      if x(i) > y(i) then return -1
      if x(i) < y(i) then return 1
      i += 1
    }
    0
  }
}

object DegreeReverseLexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new DegreeReverseLexicographic[N](variables.map(~_)*)
}
