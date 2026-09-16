package scas.power.degree

import scas.math.Numeric

trait ArrayPowerProduct[N : Numeric] extends scas.power.ArrayPowerProduct[N] {
  override def len = length + 1
  override def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
    var i = 0
    while i <= length do {
      z(i) = x.get(i) + y.get(i)
      i += 1
    }
    z
  }
  override def divide(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 to length do {
      assert (x.get(i) >= y.get(i))
      z(i) = x.get(i) - y.get(i)
    }
    z
  }
  extension (x: Array[N]) {
    override def deg = x(length)
    override def set(i: Int, c: N) = {
      x(i) = c
      x(length) += c
    }
  }
}
