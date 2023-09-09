package scas.power.splittable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.PowerProduct
import scas.util.ClassTagArray
import scas.variable.Variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](val variables: Variable*) extends impl.PowerProduct[Array[N]] with scas.power.impl.Lexicographic[N] with PowerProduct[Array[N]] {
  given instance: Lexicographic[N] = this
  def newInstance(variables: Seq[Variable]) = new Lexicographic[N](variables: _*)
}
