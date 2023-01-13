package scas.power.splittable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.splittable.PowerProduct
import scas.util.ClassTagArray
import scas.variable.Variable

class Lexicographic[N : Numeric : ClassTag : ClassTagArray](variables: Variable*) extends scas.power.Lexicographic[N](variables: _*) with PowerProduct[Array[N]] {
  def newInstance(variables: Seq[Variable]) = new Lexicographic[N](variables: _*)
}
