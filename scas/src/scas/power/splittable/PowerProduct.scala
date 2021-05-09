package scas.power.splittable

import scala.reflect.ClassTag
import scas.variable.Variable

trait PowerProduct[M: ClassTag] extends scas.power.PowerProduct[M] {
  def take(n: Int) = newInstance(variables.take(n))
  def drop(n: Int) = newInstance(variables.drop(n))
  def newInstance(variables: Seq[Variable]): PowerProduct[M]
}
