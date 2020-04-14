package scas.power

import scas.math.Numeric
import scas.variable.Variable

abstract class PowerProductWithDegree[N : Numeric : ClassTag : ClassTagArray] extends PowerProduct[N] {
  def one = new Array[N](length + 1)
  def generator(n: Int) = {
    val r = one
    for (i <- 0 to length) r(i) = numeric.fromInt(if (i == n || i == length) 1 else 0)
    r
  }
  def degree(x: Array[N]) = numeric.toLong(x(length))
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = one
    for (i <- 0 until length) {
      r(i) = numeric.min(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  def scm(x: Array[N], y: Array[N]): Array[N] = {
    val r = one
    for (i <- 0 until length) {
      r(i) = numeric.max(x(i), y(i))
      r(length) += r(i)
    }
    r
  }
  def (x: Array[N]) * (y: Array[N]) = {
    val r = one
    var i = 0
    while (i <= length) {
      r(i) = x(i) + y(i)
      i += 1
    }
    r
  }
  def (x: Array[N]) / (y: Array[N]) = {
    val r = one
    for (i <- 0 to length) {
      assert (x(i) >= y(i))
      r(i) = x(i) - y(i)
    }
    r
  }
  def (x: Array[N]) | (y: Array[N]) = {
    var i = 0
    while (i < length) {
      if (x(i) > y(i)) return false
      i += 1
    }
    true
  }
  def (x: Array[N]).projection(n: Int) = {
    val r = one
    for (i <- 0 to length) r(i) = if (i == n || i == length) x(n) else numeric.zero
    r
  }
  def (x: Array[N]).convert(from: Variable*) = {
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
  def (x: Array[N]).get(i: Int) = x(i)
}
