package scas.power.compact.degree

trait BinaryPowerProduct extends scas.power.compact.BinaryPowerProduct with PowerProduct {
  override def multiply(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    var i = 0
    while i < len - 1 do {
      z(i) = x(i) | y(i)
      z(len - 1) += java.lang.Integer.bitCount(z(i))
      i += 1
    }
    z
  }
  override def divide(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    for i <- 0 until len - 1 do {
      assert (x(i) >= y(i))
      z(i) = x(i) ^ y(i)
      z(len - 1) += java.lang.Integer.bitCount(z(i))
    }
    z
  }
  extension (x: Array[Int]) {
    override def deg = x(len - 1)
  }
}
