package scas.power.splittable

import scala.reflect.ClassTag
import scas.variable.Variable

trait PowerProduct[M: ClassTag] extends PowerProduct.Impl[M] with scas.power.PowerProduct[M]

object PowerProduct {
  trait Impl[M: ClassTag] extends scas.power.PowerProduct.Impl[M] {
    def take(n: Int) = newInstance(variables.take(n))
    def drop(n: Int) = newInstance(variables.drop(n))
    def newInstance(variables: Seq[Variable]): PowerProduct[M]
  }
}
