package scas.power.splittable

import scala.reflect.ClassTag

trait PowerProduct[M: ClassTag] extends impl.PowerProduct[M] with scas.power.PowerProduct[M]
