package scas.power.offset

import scas.power.ArrayPowerProduct

trait PowerProduct[N] extends ArrayPowerProduct[N] {
  override def compare(x: Array[N], y: Array[N]) = compare(x, 0, y, 0)
  def compare(x: Array[N], n: Int, y: Array[N], m: Int): Int
  extension (x: Array[N]) {
    override def multiply(y: Array[N]) = {
      val r = empty
      this.multiply(x, 0, y, r)
      r
    }
  }
  def multiply(x: Array[N], n: Int, y: Array[N], z: Array[N]): Unit
}
