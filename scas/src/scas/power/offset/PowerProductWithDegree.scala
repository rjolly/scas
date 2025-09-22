package scas.power.offset

import scas.math.Numeric
import scas.power.ArrayPowerProductWithDegree

trait PowerProductWithDegree[N : Numeric] extends ArrayPowerProductWithDegree[N] with PowerProduct[N] {
  def multiply(x: Array[N], n: Int, y: Array[N], z: Array[N]) = {
    val k = n * (length + 1)
    var i = 0
    while i <= length do {
      z(i + k) = x(i + k) + y(i)
      i += 1
    }
  }
}
