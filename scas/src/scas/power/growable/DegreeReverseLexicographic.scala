package scas.power.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](variables: Variable*) extends scas.power.DegreeReverseLexicographic[N](variables*) with ArrayPowerProduct[N] {
  override def newInstance(variables: Variable*) = new DegreeReverseLexicographic[N](variables*)
}

object DegreeReverseLexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new DegreeReverseLexicographic[N](variables.map(~_)*)
}
