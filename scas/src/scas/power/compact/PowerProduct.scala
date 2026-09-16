package scas.power.compact

trait PowerProduct extends scas.power.ArrayPowerProduct[Int] {
  def shift: Int
  val mask = (1 << (1 << shift)) - 1
  override def len = ((length - 1) >> (5 - shift)) + 1
  override def multiply(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    var i = 0
    while i < len do {
      z(i) = x(i) + y(i)
      i += 1
    }
    z
  }
  override def divide(x: Array[Int], y: Array[Int], z: Array[Int]) = {
    for i <- 0 until len do {
      assert (x(i) >= y(i))
      z(i) = x(i) - y(i)
    }
    z
  }
  extension (x: Array[Int]) {
    override def get(i: Int) = {
      val p = (i << shift) + ((i + (1 << shift)) >> 5)
      val q = p >> 5
      val r = p & 31
      (x(q) >> r) & mask
    }
    override def set(i: Int, c: Int) = {
      val p = (i << shift) + ((i + (1 << shift)) >> 5)
      val q = p >> 5
      val r = p & 31
      x(q) |= (c & mask) << r
    }
  }
}
