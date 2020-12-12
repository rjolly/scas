package scas.power

import scas.math.Numeric
import scas.variable.Variable
import ArrayPowerProduct.Element

object ArrayPowerProductWithDegree {
  abstract class Factory[N : Numeric : ClassTag : ClassTagArray] extends ArrayPowerProduct.Factory[N] {
    def newArray = new Array[N](length + 1)
    val one = newArray
    def generator(n: Int) = {
      val r = newArray
      for (i <- 0 to length) r(i) = numeric.fromInt(if (i == n || i == length) 1 else 0)
      r
    }
    def degree(x: Element[N]) = x(length).toLong
    def gcd(x: Element[N], y: Element[N]): Element[N] = {
      val r = newArray
      for (i <- 0 until length) {
        r(i) = numeric.min(x(i), y(i))
        r(length) += r(i)
      }
      r
    }
    def lcm(x: Element[N], y: Element[N]): Element[N] = {
      val r = newArray
      for (i <- 0 until length) {
        r(i) = numeric.max(x(i), y(i))
        r(length) += r(i)
      }
      r
    }
    extension (x: Element[N]) def * (y: Element[N]) = {
      val r = newArray
      var i = 0
      while (i <= length) {
        r(i) = x(i) + y(i)
        i += 1
      }
      r
    }
    extension (x: Element[N]) def / (y: Element[N]) = {
      val r = newArray
      for (i <- 0 to length) {
        assert (x(i) >= y(i))
        r(i) = x(i) - y(i)
      }
      r
    }
    extension (x: Element[N]) def | (y: Element[N]) = {
      var i = 0
      while (i < length) {
        if (x(i) > y(i)) return false
        i += 1
      }
      true
    }
    extension (x: Element[N]) def projection(n: Int) = {
      val r = newArray
      for (i <- 0 to length) r(i) = if (i == n || i == length) x(n) else numeric.zero
      r
    }
    extension (x: Element[N]) def convert(from: Variable*) = {
      val r = newArray
      val index = from.map(a => variables.indexOf(a))
      for (i <- 0 until x.length - 1) if (x(i) > numeric.zero) {
        val c = index(i)
        assert (c > -1)
        r(c) = x(i)
      }
      r(length) = x(x.length - 1)
      r
    }
    extension (x: Element[N]) def apply(i: Int) = x(i)
  }
}
