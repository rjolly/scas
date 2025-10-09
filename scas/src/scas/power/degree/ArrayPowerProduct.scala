package scas.power.degree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.PowerProduct

trait ArrayPowerProduct[N : {Numeric as numeric, ClassTag}] extends scas.power.ArrayPowerProduct[N] {
  override def empty = new Array[N](length + 1)
  override def generator(n: Int) = {
    val r = empty
    for i <- 0 to length do r(i) = numeric.fromInt(if i == n || i == length then 1 else 0)
    r
  }
  override def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for i <- 0 until length do {
      r(i) = numeric.min(x.get(i), y.get(i))
      r(length) += r(i)
    }
    r
  }
  override def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for i <- 0 until length do {
      r(i) = numeric.max(x.get(i), y.get(i))
      r(length) += r(i)
    }
    r
  }
  extension (x: Array[N]) {
    inline override def deg = x.get(length)
    override def multiply(y: Array[N]) = {
      val r = empty
      var i = 0
      while i <= length do {
        r(i) = x.get(i) + y.get(i)
        i += 1
      }
      r
    }
    override def divide(y: Array[N]) = {
      val r = empty
      for i <- 0 to length do {
        assert (x.get(i) >= y.get(i))
        r(i) = x.get(i) - y.get(i)
      }
      r
    }
    override def projection(n: Int, m: Int) = {
      val r = empty
      for i <- 0 until length do if i >= n && i < m then {
        r(i) = x.get(i)
        r(length) += x.get(i)
      }
      r
    }
    override def convert(from: PowerProduct[Array[N]]) = {
      val r = empty
      val l = if from == this then x.length - 1 else from.length
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until l do if from.get(x)(i) > numeric.zero then {
        val c = index(i)
        assert (c > -1)
        r(c) = from.get(x)(i)
      }
      r(length) = if from == this then x(l) else numeric.fromInt(from.degree(x).intValue)
      r
    }
  }
}
