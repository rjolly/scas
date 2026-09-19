package scas.power

import scala.reflect.ClassTag

trait ArrayPowerProduct[N : ClassTag] extends IndexedPowerProduct[Array, N] {
  def len = length
  def empty = new Array[N](len)
  val one = empty
  extension (x: Array[N]) {
    def get(i: Int) = x(i)
    def set(i: Int, c: N) = x(i) = c
  }
}
