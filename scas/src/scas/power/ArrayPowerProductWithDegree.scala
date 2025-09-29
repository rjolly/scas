package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.base.BigInteger
import BigInteger.given

trait ArrayPowerProductWithDegree[N : {Numeric as numeric, ClassTag}] extends ArrayPowerProduct[N] {
  def empty = new Array[N](length + 1)
  def generator(n: Int) = {
    val r = empty
    for i <- 0 to length do r(i) = numeric.fromInt(if i == n || i == length then 1 else 0)
    r
  }
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for i <- 0 until length do {
      r(i) = numeric.min(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for i <- 0 until length do {
      r(i) = numeric.max(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  extension (x: Array[N]) {
    def degree = BigInteger.fromInt(x(length).toLong)
    def multiply(y: Array[N]) = {
      val r = empty
      var i = 0
      while i <= length do {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
    def divide(y: Array[N]) = {
      val r = empty
      for i <- 0 to length do {
        assert (x(i) >= y(i))
        r(i) = x(i) - y(i)
      }
      r
    }
    def factorOf(y: Array[N]) = {
      var i = 0
      while i < length do {
        if x(i) > y(i) then return false
        i += 1
      }
      true
    }
    def projection(n: Int, m: Int) = {
      val r = empty
      for i <- 0 until length do if i >= n && i < m then {
        r(i) = x(i)
        r(length) += x(i)
      }
      r
    }
    def convert(from: PowerProduct[Array[N]]) = {
      val r = empty
      val l = if from == this then x.length - 1 else from.length
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until l do if x(i) > numeric.zero then {
        val c = index(i)
        assert (c > -1)
        r(c) = x(i)
      }
      r(length) = x(l)
      r
    }
  }
}
