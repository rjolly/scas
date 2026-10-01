package scas.power.compact.offset.degree

trait BinaryPowerProduct extends PowerProduct with scas.power.compact.degree.BinaryPowerProduct {
  override def multiply(x: Array[Int], n: Int, y: Array[Int], z: Array[Int]) = {
    val k = n * length
    var i = 0
    while i < length - 1 do {
      z(i + k) = x(i + k) | y(i)
      z(length - 1 + k) += java.lang.Integer.bitCount(z(i + k))
      i += 1
    }
    z
  }
}
