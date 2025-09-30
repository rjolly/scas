package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

trait ArrayPowerProduct[N : {Numeric as numeric, ClassTag}] extends PowerProduct[Array[N]] {
  def one = empty
  def empty = new Array[N](length)
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
  extension (x: Array[N]) def dependencyOnVariables = (for i <- 0 until length if (x(i) > numeric.zero) yield i).toArray
  extension (x: Array[N]) def toCode(level: Level, times: String) = {
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
  extension (x: Array[N]) def toMathML(times: String) = {
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
  extension (x: Array[N]) def size = {
    var m = 0
    for i <- 0 until length do if x.get(i) > numeric.zero then m += 1
    m
  }
  extension (x: Array[N]) {
    def degree = BigInteger.fromInt(deg.toLong)
    def deg = {
      var d = numeric.zero
      for i <- 0 until length do d += x.get(i)
      d
    }
    def get(i: Int) = x(i)
  }
}
