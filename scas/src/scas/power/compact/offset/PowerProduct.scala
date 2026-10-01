package scas.power.compact.offset

import scas.math.Numeric

trait PowerProduct extends scas.power.compact.PowerProduct with scas.power.offset.ArrayPowerProduct[Int] {
  override def multiply(x: Array[Int], n: Int, y: Array[Int], z: Array[Int]) = {
    val k = n * length
    var i = 0
    while i < length do {
      z(i + k) = x(i + k) + y(i)
      i += 1
    }
    z
  }
}
