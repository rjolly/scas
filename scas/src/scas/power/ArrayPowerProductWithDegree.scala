package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

trait ArrayPowerProductWithDegree[N : ClassTag](using numeric: Numeric[N]) extends ArrayPowerProduct[N] {
  def empty = new Array[N](length + 1)
  val one = empty
  def generator(n: Int) = {
    val r = empty
    for (i <- 0 to length) r(i) = numeric.fromInt(if (i == n || i == length) 1 else 0)
    r
  }
  def degree(x: Array[N]) = BigInteger.fromInt(x(length).toLong)
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for (i <- 0 until length) {
      r(i) = numeric.min(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for (i <- 0 until length) {
      r(i) = numeric.max(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  extension (x: Array[N]) {
    def multiply(y: Array[N]) = {
      val r = empty
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
    def divide(y: Array[N]) = {
      val r = empty
      for (i <- 0 to length) {
        assert (x(i) >= y(i))
        r(i) = x(i) - y(i)
      }
      r
    }
    def factorOf(y: Array[N]) = {
      var i = 0
      while (i < length) {
        if (x(i) > y(i)) return false
        i += 1
      }
      true
    }
    def projection(n: Int) = {
      val r = empty
      for (i <- 0 until length) if (i == n) {
        r(i) = x(i)
        r(length) = x(i)
      }
      r
    }
    def convert(from: PowerProduct[Array[N]]) = {
      val r = empty
      val index = from.variables.map(a => variables.indexOf(a))
      for (i <- 0 until from.length) if (x(i) > numeric.zero) {
        val c = index(i)
        assert (c > -1)
        r(c) = x(i)
      }
      r(length) = x(from.length)
      r
    }
  }
}
