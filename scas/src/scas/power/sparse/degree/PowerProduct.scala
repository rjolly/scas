package scas.power.sparse.degree

import scala.collection.mutable.ArrayBuffer
import scas.math.Numeric
import scas.power.ArrayPowerProduct

trait PowerProduct[N : Numeric as numeric] extends scas.power.sparse.PowerProduct[N] {
  override def length = 2
  override def generator(n: Int, buffer: ArrayBuffer[(N, N)]) = {
    super.generator(n, buffer)
    buffer.withdeg
  }
  override def gcd(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    super.gcd(x, y, buffer)
    buffer.withdeg
  }
  override def lcm(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    super.lcm(x, y, buffer)
    buffer.withdeg
  }
  override def multiply(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    super.multiply(x, y, buffer)
    buffer.withdeg(x.deg + y.deg)
  }
  override def divide(x: Array[N], y: Array[N], buffer: ArrayBuffer[(N, N)]) = {
    super.divide(x, y, buffer)
    buffer.withdeg(x.deg - y.deg)
  }
  override def projection(x: Array[N], n: Int, m: Int, buffer: ArrayBuffer[(N, N)]) = {
    super.projection(x, n, m, buffer)
    buffer.withdeg
  }
  override def convert(x: Array[N], from: ArrayPowerProduct[N], buffer: ArrayBuffer[(N, N)]) = {
    super.convert(x, from, buffer)
    assert (buffer.array.deg == x.deg)
    buffer.withdeg(x.deg)
  }
  extension (x: Array[N]) {
    override def size = super.size(x) - 1
    override def deg = x.get(x.size)
  }
  extension (buffer: ArrayBuffer[(N, N)]) {
    def withdeg(degree: N) = {
      buffer += ((numeric.fromInt(0), degree))
      buffer
    }
    def withdeg: ArrayBuffer[(N, N)] = buffer.withdeg(buffer.deg)
    def deg = {
      var d = numeric.zero
      for i <- 0 until buffer.length do d += buffer(i)._2
      d
    }
  }
}
