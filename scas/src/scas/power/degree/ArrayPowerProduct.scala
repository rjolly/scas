package scas.power.degree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.PowerProduct

trait ArrayPowerProduct[N : {Numeric as numeric, ClassTag}] extends scas.power.ArrayPowerProduct[N] {
  override def len = length + 1
  override def generator(n: Int) = {
    val r = empty
    r(n) = numeric.fromInt(1)
    r(length) = numeric.fromInt(1)
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
    override def convert(from: scas.power.ArrayPowerProduct[N]) = {
      val r = empty
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until from.length do if from.get(x)(i) > numeric.zero then {
        val c = index(i)
        assert (c > -1)
        r(c) = from.get(x)(i)
      }
      r(length) = from.deg(x)
      r
    }
  }
}
