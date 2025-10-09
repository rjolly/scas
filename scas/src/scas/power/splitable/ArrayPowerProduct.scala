package scas.power.splitable

import scas.math.Numeric

trait ArrayPowerProduct[N : Numeric as numeric] extends scas.power.ArrayPowerProduct[N] with PowerProduct[Array[N]]
