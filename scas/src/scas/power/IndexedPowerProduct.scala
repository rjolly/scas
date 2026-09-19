package scas.power

import scas.math.Numeric
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

trait IndexedPowerProduct[M[N], N : Numeric as numeric] extends PowerProduct[M[N]] {
  def empty: M[N]
  def generator(n: Int) = generator(n, empty)
  def generator(n: Int, z: M[N]) = {
    z.set(n, numeric.fromInt(1))
    z
  }
  def gcd(x: M[N], y: M[N]) = gcd(x, y, empty)
  def gcd(x: M[N], y: M[N], z: M[N]) = {
    for i <- 0 until length do {
      z.set(i, numeric.min(x.get(i), y.get(i)))
    }
    z
  }
  def lcm(x: M[N], y: M[N]) = lcm(x, y, empty)
  def lcm(x: M[N], y: M[N], z: M[N]) = {
    for i <- 0 until length do {
      z.set(i, numeric.max(x.get(i), y.get(i)))
    }
    z
  }
  def multiply(x: M[N], y: M[N], z: M[N]) = {
    var i = 0
    while i < length do {
      z.set(i, x.get(i) + y.get(i))
      i += 1
    }
    z
  }
  def divide(x: M[N], y: M[N], z: M[N]) = {
    for i <- 0 until length do {
      assert (x.get(i) >= y.get(i))
      z.set(i, x.get(i) - y.get(i))
    }
    z
  }
  def projection(x: M[N], n: Int, m: Int, z: M[N]) = {
    for i <- 0 until length do if i >= n && i < m then {
      z.set(i, x.get(i))
    }
    z
  }
  def convert(x: M[N], from: IndexedPowerProduct[M, N], z: M[N]) = {
    val index = from.variables.map(a => variables.indexOf(a))
    for i <- 0 until from.length do if from.get(x)(i) > numeric.zero then {
      val c = index(i)
      assert (c > -1)
      z.set(c, from.get(x)(i))
    }
    z
  }
  extension (x: M[N]) {
    def multiply(y: M[N]) = this.multiply(x, y, empty)
    def divide(y: M[N]) = this.divide(x, y, empty)
    def factorOf(y: M[N]) = {
      var i = 0
      while i < length do {
        if x.get(i) > y.get(i) then return false
        i += 1
      }
      true
    }
    override def projection(n: Int) = super.projection(x)(n)
    def projection(n: Int, m: Int) = this.projection(x, n, m, empty)
    def convert(from: IndexedPowerProduct[M, N]): M[N] = this.convert(x, from, empty)
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
    def get(i: Int): N
    def set(i: Int, c: N): Unit
  }
}
