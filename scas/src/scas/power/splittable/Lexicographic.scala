package scas.power.splittable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.util.unary_~

class Lexicographic[N : Numeric : ClassTag](val variables: Variable*) extends PowerProduct[Array[N]] with scas.power.Lexicographic[N] {
  def newInstance(variables: Seq[Variable]) = new Lexicographic[N](variables*)
}

object Lexicographic {
  def apply[N : Numeric : ClassTag](s: String*) = new Lexicographic[N](s.map(~_)*)
}
