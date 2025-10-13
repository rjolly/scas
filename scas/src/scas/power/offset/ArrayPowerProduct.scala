package scas.power.offset

import scas.math.Numeric

trait ArrayPowerProduct[N : Numeric] extends scas.power.ArrayPowerProduct[N] {
  override def compare(x: Array[N], y: Array[N]) = compare(x, 0, y, 0)
  def compare(x: Array[N], n: Int, y: Array[N], m: Int): Int
  override def multiply(x: Array[N], y: Array[N], z: Array[N]) = multiply(x, 0, y, z)
  def multiply(x: Array[N], n: Int, y: Array[N], z: Array[N]) = {
    val k = n * length
    var i = 0
    while i < length do {
      z(i + k) = x(i + k) + y(i)
      i += 1
    }
    z
  }
}
