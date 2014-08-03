package scas.power.splittable

trait PowerProductWithDegree[@specialized(Byte, Short, Int, Long) N] extends PowerProduct[N] with scas.power.PowerProductWithDegree[N]
