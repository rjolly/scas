package scas.power.degree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.PowerProduct

trait ArrayPowerProduct[N : {Numeric as numeric, ClassTag}] extends scas.power.ArrayPowerProduct[N] {
  override def len = length + 1
  override def generator(n: Int, z: Array[N]) = {
    z(n) = numeric.fromInt(1)
    z(length) = numeric.fromInt(1)
    z
  }
  override def gcd(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 until length do {
      z(i) = numeric.min(x.get(i), y.get(i))
      z(length) += z(i)
    }
    z
  }
  override def lcm(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 until length do {
      z(i) = numeric.max(x.get(i), y.get(i))
      z(length) += z(i)
    }
    z
  }
  override def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
    var i = 0
    while i <= length do {
      z(i) = x.get(i) + y.get(i)
      i += 1
    }
    z
  }
  override def divide(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 to length do {
      assert (x.get(i) >= y.get(i))
      z(i) = x.get(i) - y.get(i)
    }
    z
  }
  override def projection(x: Array[N], n: Int, m: Int, z: Array[N]) = {
    for i <- 0 until length do if i >= n && i < m then {
      z(i) = x.get(i)
      z(length) += x.get(i)
    }
    z
  }
  override def convert(x: Array[N], from: scas.power.ArrayPowerProduct[N], z: Array[N]) = {
    val index = from.variables.map(a => variables.indexOf(a))
    for i <- 0 until from.length do if from.get(x)(i) > numeric.zero then {
      val c = index(i)
      assert (c > -1)
      z(c) = from.get(x)(i)
    }
    z(length) = from.deg(x)
    z
  }
  extension (x: Array[N]) {
    inline override def deg = x.get(length)
  }
}
