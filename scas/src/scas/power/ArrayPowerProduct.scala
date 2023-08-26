package scas.power

import scas.math.Numeric
import scas.util.ClassTagArray

trait ArrayPowerProduct[N : Numeric : ClassTagArray] extends impl.ArrayPowerProduct[N] with PowerProduct[Array[N]]
