package scas.power.splittable

import scala.reflect.ClassTag
import scas.Variable
import scas.math.Numeric

trait PowerProduct[@specialized(Byte, Short, Int, Long) N] extends scas.power.PowerProduct[N] {
  def take(n: Int) = instance(variables.take(n))
  def drop(n: Int) = instance(variables.drop(n))
  def instance(variables: Array[Variable]): PowerProduct[N]
}

object PowerProduct {
  def apply(variables: Variable*) = lexicographic[Int](variables: _*)
  def lexicographic[@specialized(Byte, Short, Int, Long) N](variables: Variable*)(implicit nm: Numeric[N], m: ClassTag[N], cm: ClassTag[Array[N]]) = new Lexicographic[N](variables.toArray)
}
