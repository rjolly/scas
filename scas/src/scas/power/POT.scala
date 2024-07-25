package scas.power

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

class POT[N : ClassTag](using numeric: Numeric[N])(factory: ArrayPowerProduct[N], name: String, dimension: Int) extends ArrayPowerProductWithDegree[N] {
  val variables = factory.variables ++ (for (i <- 0 until dimension) yield Variable(name, Array(i)*))
  def newInstance(variables: Variable*) = new POT(factory.newInstance(variables*), name, dimension)

  def compare(x: Array[N], y: Array[N]) = {
    var i = 0
    while (i < dimension) {
      if (x(length + 1 + i) < y(length + 1 + i)) return -1
      if (x(length + 1 + i) > y(length + 1 + i)) return 1
      i += 1
    }
    factory.compare(x, y)
  }
  override def length = factory.length
  override def empty = new Array[N](length + 1 + dimension)
  override def generator(n: Int) = if (n < length) super.generator(n) else {
    val r = empty
    r(n + 1) = numeric.fromInt(1)
    r
  }
  override def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = super.gcd(x, y)
    for (i <- 0 until dimension) {
      r(length + 1 + i) = numeric.min(x(length + 1 + i), y(length + 1 + i))
    }
    r
  }
  override def lcm(x: Array[N], y: Array[N]): Array[N] = {
    val r = super.lcm(x, y)
    for (i <- 0 until dimension) {
      r(length + 1 + i) = numeric.max(x(length + 1 + i), y(length + 1 + i))
    }
    r
  }
  extension (x: Array[N]) {
    override def multiply(y: Array[N]) = {
      val r = super.multiply(x)(y)
      var i = 0
      while (i < dimension) {
        r(length + 1 + i) = x(length + 1 + i) + y(length + 1 + i)
        i += 1
      }
      r
    }
    override def divide(y: Array[N]) = {
      val r = super.divide(x)(y)
      for (i <- 0 until dimension) {
        assert (x(length + 1 + i) >= y(length + 1 + i))
        r(length + 1 + i) = x(length + 1 + i) - y(length + 1 + i)
      }
      r
    }
    override def factorOf(y: Array[N]) = {
      if (!super.factorOf(x)(y)) return false
      var i = 0
      while (i < dimension) {
        if (x(length + 1 + i) > y(length + 1 + i)) return false
        i += 1
      }
      true
    }
    override def projection(n: Int, m: Int) = {
      val r = super.projection(x)(n, m)
      for (i <- 0 until dimension) if (length + i >= n && length + i < m) {
        r(length + 1 + i) = x(length + 1 + i)
      }
      r
    }
  }
  override def dependencyOnVariables(x: Array[N]) = super.dependencyOnVariables(x) ++ (for (i <- 0 until dimension if (x(length + 1 + i) > numeric.zero)) yield length + i).toArray
  extension (x: Array[N]) override def toCode(level: Level) = {
    var s = super.toCode(x)(level)
    var m = super.size(x)
    for (i <- 0 until dimension) if (x(length + 1 + i) > numeric.zero) {
      val a = variables(length + i)
      val b = x(length + 1 + i)
      val t = if (b >< numeric.one) a.toString else s"$a\\$b"
      s = if (m == 0) t else s + "*" + t
      m += 1
    }
    s
  }
  extension (x: Array[N]) override def toMathML = {
    var s = super.toMathML(x)
    var m = super.size(x)
    for (i <- 0 until dimension) if (x(length + 1 + i) > numeric.zero) {
      val a = variables(length + i)
      val b = x(length + 1 + i)
      val t = if (b >< numeric.one) a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
      s = if (m == 0) t else s"<apply><times/>$s$t</apply>"
      m += 1
    }
    s
  }
  override def size(x: Array[N]) = {
    var m = super.size(x)
    for (i <- 0 until dimension) if (x(length + 1 + i) > numeric.zero) m += 1
    m
  }
}
