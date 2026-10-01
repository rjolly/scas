package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

open class POT[N : {Numeric as numeric, ClassTag}](factory: ArrayPowerProduct[N], dimension: Int)(val variables: Variable*) extends ArrayPowerProduct[N] {
  def this(factory: ArrayPowerProduct[N], name: String, dimension: Int) = this(factory, dimension)(factory.variables ++ (for i <- 0 until dimension yield Variable(name, 0, Array(i)*))*)

  def compare(x: Array[N], y: Array[N]) = {
    var i = 0
    while i < dimension do {
      if x(factory.length + i) < y(factory.length + i) then return -1
      if x(factory.length + i) > y(factory.length + i) then return 1
      i += 1
    }
    factory.compare(x, y)
  }
  override def length = factory.length + dimension
  override def generator(n: Int, z: Array[N]) = {
    if n < factory.nbvars then factory.generator(n, z)
    else z(factory.length + n - factory.nbvars) = numeric.fromInt(1)
    z
  }
  override def gcd(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.gcd(x, y, z)
    for i <- 0 until dimension do {
      z(factory.length + i) = numeric.min(x(factory.length + i), y(factory.length + i))
    }
    z
  }
  override def lcm(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.lcm(x, y, z)
    for i <- 0 until dimension do {
      z(factory.length + i) = numeric.max(x(factory.length + i), y(factory.length + i))
    }
    z
  }
  override def multiply(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.multiply(x, y, z)
    var i = 0
    while i < dimension do {
      z(factory.length + i) = x(factory.length + i) + y(factory.length + i)
      i += 1
    }
    z
  }
  override def divide(x: Array[N], y: Array[N], z: Array[N]) = {
    factory.divide(x, y, z)
    for i <- 0 until dimension do {
      assert (x(factory.length + i) >= y(factory.length + i))
      z(factory.length + i) = x(factory.length + i) - y(factory.length + i)
    }
    z
  }
  override def projection(x: Array[N], n: Int, m: Int, z: Array[N]) = {
    factory.projection(x, n, m, z)
    for i <- 0 until dimension do if factory.nbvars + i >= n && factory.nbvars + i < m then {
      z(factory.length + i) = x(factory.length + i)
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
        if x(factory.length + i) > y(factory.length + i) then return false
        i += 1
      }
      true
    }
    override def dependencyOnVariables = factory.dependencyOnVariables(x) ++ (for i <- 0 until dimension if (x(factory.length + i) > numeric.zero) yield factory.nbvars + i).toArray
    override def size = {
      var m = factory.size(x)
      for i <- 0 until dimension do if x(factory.length + i) > numeric.zero then m += 1
      m
    }
    override def deg = {
      var d = factory.deg(x)
      for i <- 0 until dimension do d += x(factory.length + i)
      d
    }
    override def toCode(level: Level, times: String) = {
      var s = factory.toCode(x)(level, times)
      var m = factory.size(x)
      for i <- 0 until dimension do if x(factory.length + i) > numeric.zero then {
        val a = variables(factory.nbvars + i)
        val b = x(factory.length + i)
        val t = if b >< numeric.one then a.toString else s"$a\\$b"
        s = if m == 0 then t else s + times + t
        m += 1
      }
      s
    }
    override def toMathML(times: String) = {
      var s = factory.toMathML(x)(times)
      var m = factory.size(x)
      for i <- 0 until dimension do if x(factory.length + i) > numeric.zero then {
        val a = variables(factory.nbvars + i)
        val b = x(factory.length + i)
        val t = if b >< numeric.one then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if m == 0 then t else s"<apply><$times/>$s$t</apply>"
        m += 1
      }
      s
    }
  }
}
