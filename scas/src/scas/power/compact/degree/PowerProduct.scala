package scas.power.compact.degree

trait PowerProduct extends scas.power.compact.PowerProduct with scas.power.degree.ArrayPowerProduct[Int] {
  override def len = super[PowerProduct].len + 1
  override def multiply(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    var i = 0
    while i <= len - 1 do {
      z(i) = x(i) + y(i)
      i += 1
    }
    z
  }
  override def divide(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    for i <- 0 to len - 1 do {
      assert (x(i) >= y(i))
      z(i) = x(i) - y(i)
    }
    z
  }
  extension (x: Array[Int]) {
    override def deg = x(len - 1)
    override def set(i: Int, c: Int) = {
      super[PowerProduct].set(x)(i, c)
      x(len - 1) += c
    }
  }
}
