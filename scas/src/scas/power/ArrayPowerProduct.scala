package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

trait ArrayPowerProduct[N : {Numeric as numeric, ClassTag}] extends PowerProduct[Array[N]] {
  val one = empty
  def len = length
  def empty = new Array[N](len)
  def generator(n: Int) = generator(n, empty)
  def generator(n: Int, z: Array[N]) = {
    z(n) = numeric.fromInt(1)
    z
  }
  def gcd(x: Array[N], y: Array[N]) = gcd(x, y, empty)
  def gcd(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 until length do {
      z(i) = numeric.min(x.get(i), y.get(i))
    }
    z
  }
  def lcm(x: Array[N], y: Array[N]) = lcm(x, y, empty)
  def lcm(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 until length do {
      z(i) = numeric.max(x.get(i), y.get(i))
    }
    z
  }
  def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
    var i = 0
    while i < length do {
      z(i) = x.get(i) + y.get(i)
      i += 1
    }
    z
  }
  def divide(x: Array[N], y: Array[N], z: Array[N]) = {
    for i <- 0 until length do {
      assert (x.get(i) >= y.get(i))
      z(i) = x.get(i) - y.get(i)
    }
    z
  }
  def projection(x: Array[N], n: Int, m: Int, z: Array[N]) = {
    for i <- 0 until length do if i >= n && i < m then {
      z(i) = x.get(i)
    }
    z
  }
  def convert(x: Array[N], from: ArrayPowerProduct[N], z: Array[N]) = {
    val index = from.variables.map(a => variables.indexOf(a))
    for i <- 0 until from.length do if from.get(x)(i) > numeric.zero then {
      val c = index(i)
      assert (c > -1)
      z(c) = from.get(x)(i)
    }
    z
  }
  extension (x: Array[N]) {
    def multiply(y: Array[N]) = this.multiply(x, y, empty)
    def divide(y: Array[N]) = this.divide(x, y, empty)
    def factorOf(y: Array[N]) = {
      var i = 0
      while i < length do {
        if x.get(i) > y.get(i) then return false
        i += 1
      }
      true
    }
    def projection(n: Int, m: Int) = this.projection(x, n, m, empty)
    def convert(from: ArrayPowerProduct[N]): Array[N] = this.convert(x, from, empty)
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
