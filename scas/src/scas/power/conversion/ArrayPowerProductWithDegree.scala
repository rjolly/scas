package scas.power.conversion

import scas.math.Numeric
import scas.variable.Variable

abstract class ArrayPowerProductWithDegree[N : Numeric : ClassTag : ClassTagArray] extends ArrayPowerProduct[N] {
  def one = new Array[N](length + 1)
  def generator(n: Int) = {
    val r = one
    for (i <- 0 to length) r(i) = numeric.fromInt(if (i == n || i == length) 1 else 0)
    r
  }
  def degree(x: Array[N]) = x(length).toLong
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = one
    for (i <- 0 until length) {
      r(i) = numeric.min(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = one
    for (i <- 0 until length) {
      r(i) = numeric.max(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  extension (x: Array[N]) def multiply(y: Array[N]) = {
    val r = one
    var i = 0
    while (i <= length) {
      r(i) = x(i) + y(i)
      i += 1
    }
    r
  }
  extension (x: Array[N]) def divide(y: Array[N]) = {
    val r = one
    for (i <- 0 to length) {
      assert (x(i) >= y(i))
      r(i) = x(i) - y(i)
    }
    r
  }
  extension (x: Array[N]) def factorOf(y: Array[N]) = {
    var i = 0
    while (i < length) {
      if (x(i) > y(i)) return false
      i += 1
    }
    true
  }
  extension (x: Array[N]) def projection(n: Int) = {
    val r = one
    for (i <- 0 to length) r(i) = if (i == n || i == length) x(n) else numeric.zero
    r
  }
  extension (x: Array[N]) def convert(from: Variable*) = {
    val r = one
    val index = from.map(a => variables.indexOf(a))
    for (i <- 0 until x.length - 1) if (x(i) > numeric.zero) {
      val c = index(i)
      assert (c > -1)
      r(c) = x(i)
    }
    r(length) = x(x.length - 1)
    r
  }
}
