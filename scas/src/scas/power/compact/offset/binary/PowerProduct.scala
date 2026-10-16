package scas.power.compact.offset.binary

trait PowerProduct extends scas.power.compact.offset.PowerProduct with scas.power.compact.binary.PowerProduct {
  override def multiply(x: Array[Int], n: Int, y: Array[Int], z: Array[Int]) = {
    val k = n * length
    var i = 0
    while i < length do {
      z(i + k) = x(i + k) | y(i + k)
      i += 1
    }
    z
  }
}
