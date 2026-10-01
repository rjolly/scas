package scas.power.offset.degree

import scas.math.Numeric

trait ArrayPowerProduct[N : Numeric] extends scas.power.offset.ArrayPowerProduct[N] with scas.power.degree.ArrayPowerProduct[N] {
  override def multiply(x: Array[N], n: Int, y: Array[N], z: Array[N]) = {
    val k = n * length
    var i = 0
    while i <= nbvars do {
      z(i + k) = x(i + k) + y(i)
      i += 1
    }
    z
  }
  extension (x: Array[N]) {
    def deg(n: Int) = x(n * length + nbvars)
  }
}
