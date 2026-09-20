package scas.power.compact

trait BinaryPowerProduct extends PowerProduct {
  override def multiply(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    var i = 0
    while i < len do {
      z(i) = x(i) | y(i)
      i += 1
    }
    z
  }
  override def divide(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    for i <- 0 until len do {
      assert (x(i) >= y(i))
      z(i) = x(i) ^ y(i)
    }
    z
  }
  extension (x: Array[Int]) {
    override def deg = {
      var d = 0
      for i <- 0 until len do d += java.lang.Integer.bitCount(x.get(i))
      d
    }
  }
}
