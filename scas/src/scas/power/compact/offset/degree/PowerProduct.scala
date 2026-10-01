package scas.power.compact.offset.degree

trait PowerProduct extends scas.power.compact.offset.PowerProduct with scas.power.compact.degree.PowerProduct with scas.power.offset.degree.ArrayPowerProduct[Int] {
  override def multiply(x: Array[Int], n: Int, y: Array[Int], z: Array[Int]) = {
    val k = n * length
    var i = 0
    while i < length do {
      z(i + k) = x(i + k) + y(i)
      i += 1
    }
    z
  }
  extension (x: Array[Int]) {
    override def deg(n: Int) = x(n * length + length - 1)
  }
}
