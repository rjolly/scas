package scas.power.growable.splitable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

abstract class ArrayPowerProduct[N : {Numeric as numeric, ClassTag}](degree: N)(variables: Variable*) extends scas.power.growable.ArrayPowerProduct[N](degree)(variables*) with scas.power.splitable.ArrayPowerProduct[N] {
  override def newInstance(variables: Variable*): ArrayPowerProduct[N]
}
