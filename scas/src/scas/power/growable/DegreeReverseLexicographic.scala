package scas.power.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.{Conversion, unary_~}

class DegreeReverseLexicographic[N : {Numeric, ClassTag}](var variables: Variable*) extends scas.power.DegreeReverseLexicographic.Impl[N] with ArrayPowerProduct[N] {
  def extend(variables: Variable*): Unit = this.variables ++= variables
}

object DegreeReverseLexicographic {
  def apply[N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(variables: S*) = new DegreeReverseLexicographic[N](variables.map(~_)*)
}
