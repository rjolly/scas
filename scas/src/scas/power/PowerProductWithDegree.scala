package scas.power

import scas.math.Numeric

abstract class PowerProductWithDegree[N : Numeric : ClassTag : ClassTagArray](variables: Array[String]) extends PowerProduct[N](variables) {
  def one = new Array[N](length + 1)
  def generator(n: Int) = (for (i <- 0 until length + 1) yield Numeric[N].fromInt(if (i == n || i == length) 1 else 0)).toArray
  def degree(x: Array[N]) = Numeric[N].toLong(x(x.length - 1))
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = one
    for (i <- 0 until length) r(i) = Numeric[N].min(x.get(i), y.get(i))
    r(length) = r.foldLeft(Numeric[N].zero) { (s, l) => s + l }
    r
  }
  def scm(x: Array[N], y: Array[N]): Array[N] = {
    val r = one
    for (i <- 0 until length) r(i) = Numeric[N].max(x.get(i), y.get(i))
    r(length) = r.foldLeft(Numeric[N].zero) { (s, l) => s + l }
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
    for (i <- 0 until length) {
      assert (x.get(i) >= y.get(i))
      r(i) = x.get(i) - y.get(i)
    }
    r(length) = x(x.length - 1) - y(y.length - 1)
    r
  }
  def (x: Array[N]) | (y: Array[N]) = {
    var i = 0
    while (i < length) {
      if (x.get(i) > y.get(i)) return false
      i += 1
    }
    true
  }
  def (x: Array[N]).projection(n: Int) = (for (i <- 0 until x.length) yield if (i == n || i == x.length - 1) x(n) else Numeric[N].zero).toArray
  def converter(from: Array[String]): Array[N] => Array[N] = { x =>
    val r = one
    val index = from map { a => variables.indexOf(a) }
    for (i <- 0 until x.length - 1 if (x(i) > Numeric[N].zero)) {
      val c = index(i)
      assert (c > -1)
      r(c) = x(i)
    }
    r(length) = x(x.length - 1)
    r
  }
  def (x: Array[N]).get(i: Int) = x(i)
}
