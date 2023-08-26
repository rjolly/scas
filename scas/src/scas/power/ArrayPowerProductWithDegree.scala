package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.util.ClassTagArray

trait ArrayPowerProductWithDegree[N : Numeric : ClassTag : ClassTagArray] extends impl.ArrayPowerProductWithDegree[N] with ArrayPowerProduct[N]
