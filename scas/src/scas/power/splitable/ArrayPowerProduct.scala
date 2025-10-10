package scas.power.splitable

import scas.math.Numeric

trait ArrayPowerProduct[N : Numeric as numeric] extends scas.power.ArrayPowerProduct[N] with PowerProduct[Array[N]] {
  extension (x: Array[N]) {
    def convert(from: PowerProduct[Array[N]]) = {
      val r = empty
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until from.length do if x(i) > numeric.zero then {
        val c = index(i)
        assert (c > -1)
        r(c) = x(i)
      }
      r
    }
  }
}
