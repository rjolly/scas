package scas.power.splittable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.PowerProduct
import scas.variable.Variable
import scas.util.unary_~

class Lexicographic[N : Numeric : ClassTag](val variables: Variable*) extends impl.PowerProduct[Array[N]] with scas.power.impl.Lexicographic[N] with PowerProduct[Array[N]] {
  given instance: Lexicographic[N] = this
  def newInstance(variables: Seq[Variable]) = new Lexicographic[N](variables: _*)
}

object Lexicographic {
  def apply[N : Numeric : ClassTag](degree: N)(s: String*) = new Lexicographic[N](s.map(~_): _*)
}
