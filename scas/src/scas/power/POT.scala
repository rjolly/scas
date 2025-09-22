package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable

class POT[N : {Numeric as numeric, ClassTag}](factory: ArrayPowerProduct[N], name: String, dimension: Int) extends ArrayPowerProductWithDegree[N] {
  val variables = factory.variables ++ (for i <- 0 until dimension yield Variable(name, 0, Array(i)*))
  def newInstance(variables: Variable*) = new POT(factory.newInstance(variables*), name, dimension)

  def compare(x: Array[N], y: Array[N]) = {
    var i = 0
    while i < dimension do {
      if x(length + 1 + i) < y(length + 1 + i) then return -1
      if x(length + 1 + i) > y(length + 1 + i) then return 1
      i += 1
    }
    factory.compare(x, y)
  }
  override def length = factory.length
  override def empty = new Array[N](length + 1 + dimension)
  override def generator(n: Int) = if n < length then super.generator(n) else {
    val r = empty
    r(n + 1) = numeric.fromInt(1)
    r
  }
  override def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = super.gcd(x, y)
    for i <- 0 until dimension do {
      r(length + 1 + i) = numeric.min(x(length + 1 + i), y(length + 1 + i))
    }
    r
  }
  override def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = super.lcm(x, y)
    for i <- 0 until dimension do {
      r(length + 1 + i) = numeric.max(x(length + 1 + i), y(length + 1 + i))
    }
    r
  }
  extension (x: Array[N]) {
    override def multiply(y: Array[N]) = {
      val r = super.multiply(x)(y)
      var i = 0
      while i < dimension do {
        r(length + 1 + i) = x(length + 1 + i) + y(length + 1 + i)
        i += 1
      }
      r
    }
    override def divide(y: Array[N]) = {
      val r = super.divide(x)(y)
      for i <- 0 until dimension do {
        assert (x(length + 1 + i) >= y(length + 1 + i))
        r(length + 1 + i) = x(length + 1 + i) - y(length + 1 + i)
      }
      r
    }
    override def factorOf(y: Array[N]) = {
      if !super.factorOf(x)(y) then return false
      var i = 0
      while i < dimension do {
        if x(length + 1 + i) > y(length + 1 + i) then return false
        i += 1
      }
      true
    }
    override def dependencyOnVariables = super.dependencyOnVariables(x) ++ (for i <- 0 until dimension if (x(length + 1 + i) > numeric.zero) yield length + i).toArray
    override def projection(n: Int, m: Int) = {
      val r = super.projection(x)(n, m)
      for i <- 0 until dimension do if length + i >= n && length + i < m then {
        r(length + 1 + i) = x(length + 1 + i)
      }
      r
    }
    override def size = {
      var m = super.size(x)
      for i <- 0 until dimension do if x(length + 1 + i) > numeric.zero then m += 1
      m
    }
    override def toCode(level: Level, times: String) = {
      var s = super.toCode(x)(level, times)
      var m = super.size(x)
      for i <- 0 until dimension do if x(length + 1 + i) > numeric.zero then {
        val a = variables(length + i)
        val b = x(length + 1 + i)
        val t = if b >< numeric.one then a.toString else s"$a\\$b"
        s = if m == 0 then t else s + times + t
        m += 1
      }
      s
    }
    override def toMathML(times: String) = {
      var s = super.toMathML(x)(times)
      var m = super.size(x)
      for i <- 0 until dimension do if x(length + 1 + i) > numeric.zero then {
        val a = variables(length + i)
        val b = x(length + 1 + i)
        val t = if b >< numeric.one then a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
        s = if m == 0 then t else s"<apply><$times/>$s$t</apply>"
        m += 1
      }
      s
    }
  }
}
