package scas.power.offset

import scala.reflect.ClassTag
import scas.Variable
import scas.math.Numeric

trait PowerProduct[@specialized(Byte, Short, Int, Long) N] extends scas.power.PowerProduct[N] {
  override def times(x: Array[N], y: Array[N]) = times(x, 0, y, 0)
  def times(x: Array[N], k: Int, y: Array[N], l: Int): Array[N]
  def compare(x: Array[N], y: Array[N]) = compare(x, 0, y, 0)
  def compare(x: Array[N], k: Int, y: Array[N], l: Int): Int
}

object PowerProduct {
  def apply(variables: Variable*) = lexicographic[Int](variables: _*)
  def lexicographic[@specialized(Byte, Short, Int, Long) N](variables: Variable*)(implicit nm: Numeric[N], m: ClassTag[N], cm: ClassTag[Array[N]]) = new Lexicographic[N](variables.toArray)
}
