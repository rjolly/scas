package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

open class POT[N : {Numeric as numeric, ClassTag}](factory: ArrayPowerProduct[N], dimension: Int)(val variables: Variable*) extends ArrayPowerProduct[N] {
  def this(factory: ArrayPowerProduct[N], name: String, dimension: Int) = this(factory, dimension)(factory.variables ++ (for i <- 0 until dimension yield Variable(name, 0, Array(i)*))*)

  def compare(x: Array[N], y: Array[N]) = {
    var i = 0
    while i < dimension do {
      if x(factory.len + i) < y(factory.len + i) then return -1
      if x(factory.len + i) > y(factory.len + i) then return 1
      i += 1
    }
    factory.compare(x, y)
  }
  override def len = factory.len + dimension
  override def generator(n: Int, z: Array[N]) = {
    if n < factory.length then factory.generator(n, z)
    else z(factory.len + n - factory.length) = numeric.fromInt(1)
    z
  }
  override def gcd(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.gcd(x, y, z)
    for i <- 0 until dimension do {
      z(factory.len + i) = numeric.min(x(factory.len + i), y(factory.len + i))
    }
    z
  }
  override def lcm(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.lcm(x, y, z)
    for i <- 0 until dimension do {
      z(factory.len + i) = numeric.max(x(factory.len + i), y(factory.len + i))
    }
    z
  }
  override def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.multiply(x, y, z)
    var i = 0
    while i < dimension do {
      z(factory.len + i) = x(factory.len + i) + y(factory.len + i)
      i += 1
    }
    z
  }
  override def divide(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.divide(x, y, z)
    for i <- 0 until dimension do {
      assert (x(factory.len + i) >= y(factory.len + i))
      z(factory.len + i) = x(factory.len + i) - y(factory.len + i)
    }
    z
  }
  override def projection(x: Array[N], n: Int, m: Int, z: Array[N]) = {
    factory.projection(x, n, m, z)
    for i <- 0 until dimension do if factory.length + i >= n && factory.length + i < m then {
      z(factory.len + i) = x(factory.len + i)
    }
    z
  }
  override def convert(x: Array[N], from: ArrayPowerProduct[N], z: Array[N]) = {
    factory.convert(x, from, z)
    z
  }
  extension (x: Array[N]) {
    override def factorOf(y: Array[N]) = {
      if !factory.factorOf(x)(y) then return false
      var i = 0
      while i < dimension do {
        if x(factory.len + i) > y(factory.len + i) then return false
        i += 1
      }
      true
    }
    override def dependencyOnVariables = factory.dependencyOnVariables(x) ++ (for i <- 0 until dimension if (x(factory.len + i) > numeric.zero) yield factory.length + i).toArray
    override def size = {
      var m = factory.size(x)
      for i <- 0 until dimension do if x(factory.len + i) > numeric.zero then m += 1
      m
    }
    override def deg = {
      var d = factory.deg(x)
      for i <- 0 until dimension do d += x(factory.len + i)
      d
    }
    override def toCode(level: Level, times: String) = {
      var s = factory.toCode(x)(level, times)
      var m = factory.size(x)
      for i <- 0 until dimension do if x(factory.len + i) > numeric.zero then {
        val a = variables(factory.length + i)
        val b = x(factory.len + i)
        val t = if b >< numeric.one then a.toString else s"$a\\$b"
        s = if m == 0 then t else s + times + t
        m += 1
      }
      s
    }
    override def toMathML(times: String) = {
      var s = factory.toMathML(x)(times)
      var m = factory.size(x)
      for i <- 0 until dimension do if x(factory.len + i) > numeric.zero then {
        val a = variables(factory.length + i)
        val b = x(factory.len + i)
        val t = if b >< numeric.one then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if m == 0 then t else s"<apply><$times/>$s$t</apply>"
        m += 1
      }
      s
    }
  }
}
