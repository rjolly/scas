package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

trait ArrayPowerProduct[N : {Numeric as numeric, ClassTag}] extends PowerProduct[Array[N]] {
  def one = empty
  def empty = new Array[N](length)
  def generator(n: Int) = {
    val r = empty
    for i <- 0 until length do r(i) = numeric.fromInt(if i == n then 1 else 0)
    r
  }
  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for i <- 0 until length do {
      r(i) = numeric.min(x.get(i), y.get(i))
    }
    r
  }
  def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = empty
    for i <- 0 until length do {
      r(i) = numeric.max(x.get(i), y.get(i))
    }
    r
  }
  extension (x: Array[N]) {
    def multiply(y: Array[N]) = {
      val r = empty
      var i = 0
      while i < length do {
        r(i) = x.get(i) + y.get(i)
        i += 1
      }
      r
    }
    def divide(y: Array[N]) = {
      val r = empty
      for i <- 0 until length do {
        assert (x.get(i) >= y.get(i))
        r(i) = x.get(i) - y.get(i)
      }
      r
    }
    def factorOf(y: Array[N]) = {
      var i = 0
      while i < length do {
        if x.get(i) > y.get(i) then return false
        i += 1
      }
      true
    }
    def projection(n: Int, m: Int) = {
      val r = empty
      for i <- 0 until length do if i >= n && i < m then {
        r(i) = x.get(i)
      }
      r
    }
    def convert(from: ArrayPowerProduct[N]) = {
      val r = empty
      val index = from.variables.map(a => variables.indexOf(a))
      for i <- 0 until from.length do if from.get(x)(i) > numeric.zero then {
        val c = index(i)
        assert (c > -1)
        r(c) = from.get(x)(i)
      }
      r
    }
    def dependencyOnVariables = (for i <- 0 until length if (x.get(i) > numeric.zero) yield i).toArray
    def toCode(level: Level, times: String) = {
      var s = "1"
      var m = 0
      for i <- 0 until length do if x.get(i) > numeric.zero then {
        val a = variables(i)
        val b = x.get(i)
        val t = if b >< numeric.one then a.toString else s"$a\\$b"
        s = if m == 0 then t else s + times + t
        m += 1
      }
      s
    }
    def toMathML(times: String) = {
      var s = "<cn>1</cn>"
      var m = 0
      for i <- 0 until length do if x.get(i) > numeric.zero then {
        val a = variables(i)
        val b = x.get(i)
        val t = if b >< numeric.one then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if m == 0 then t else s"<apply><$times/>$s$t</apply>"
        m += 1
      }
      s
    }
    def size = {
      var m = 0
      for i <- 0 until length do if x.get(i) > numeric.zero then m += 1
      m
    }
    def degree = BigInteger.fromInt(deg.toLong)
    def deg = {
      var d = numeric.zero
      for i <- 0 until length do d += x.get(i)
      d
    }
    def get(i: Int) = x(i)
  }
}
