package scas.power.conversion

import scala.reflect.ClassTag
import scas.structure.ordered.conversion.Monoid

trait PowerProduct[M: ClassTag] extends scas.power.PowerProduct[M] with Monoid[M] {
  given PowerProduct[M] = this

  given int2powerProduct: (Int => M) = apply(_)
}
