package scas.power.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](degree: N)(variables: Variable*) extends ArrayPowerProduct(degree)(variables*) with scas.power.DegreeReverseLexicographic.Impl[N]

object DegreeReverseLexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new DegreeReverseLexicographic(degree)(variables.map(~_)*)
}
