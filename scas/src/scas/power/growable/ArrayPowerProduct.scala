package scas.power.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

abstract class ArrayPowerProduct[N : {Numeric as numeric, ClassTag}](var variables: Variable*) extends scas.power.ArrayPowerProduct[N] with PowerProduct[Array[N]] {
  def extend(variables: Variable*): Unit = this.variables ++= variables
  extension (x: Array[N]) override def get(i: Int) = {
    if i < x.length then x(i) else numeric.zero
  }
}
